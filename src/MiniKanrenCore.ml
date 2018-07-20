(*
 * MiniKanrenCode: miniKanren implementation.
 * Copyright (C) 2015-2017
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Printf

module Log =
  struct

    type t = < name    : string;
               count   : int;
               elapsed : float;
               enter   : unit;
               leave   : unit;
               subs    : t list;
               attach  : t -> unit;
               clear   : unit
             >

    class tc (name : string) =
      object
        val mutable count   = 0
        val mutable elapsed = float_of_int 0
        val mutable origin  = float_of_int 0
        val mutable depth   = 0
        val mutable subs    = ([] : t list)
        method name     = name
        method elapsed  = elapsed
        method count    = count
        method subs     = subs
        method attach l = subs <- l :: subs
        method clear    =
          count   <- 0;
          elapsed <- float_of_int 0;
          depth   <- 0;
          List.iter (fun l -> l#clear) subs
        method enter    =
          count <- count + 1;
          depth <- depth + 1;
          origin <- Unix.((times ()).tms_utime)
        method leave   =
          if depth > 0
          then elapsed <- elapsed +. Unix.((times ()).tms_utime) -. origin
          else failwith (sprintf "OCanren fatal (Log.leave): zero depth");
          depth <- depth - 1
      end

    let create parent name =
      let this = new tc name in
      match parent with
      | Some p -> p#attach this; this
      | None   -> this

    let run   = create None "run"
    let unify = create (Some run) "unify"

    let report () =
      let buf      = Buffer.create 1024      in
      let append s = Buffer.add_string buf s in
      let rec show o l =
        append @@ sprintf "%s%s: count=%d, time=%f\n" o l#name l#count l#elapsed;
        List.iter (show (o ^ "  ")) l#subs
      in
      show "" run;
      Buffer.contents buf

    let clear () = run#clear

  end

module Stream =
  struct
    type 'a t =
      | Nil
      | Cons of 'a * ('a t)
      | Thunk  of 'a thunk
      | Waiting of 'a suspended list
    and 'a thunk =
      unit -> 'a t
    and 'a suspended =
      {is_ready: unit -> bool; zz: 'a thunk}

    let nil         = Nil
    let single x    = Cons (x, Nil)
    let cons x s    = Cons (x, s)
    let from_fun zz = Thunk zz

    let suspend ~is_ready f = Waiting [{is_ready; zz=f}]

    let rec of_list = function
    | []    -> Nil
    | x::xs -> Cons (x, of_list xs)

    let force = function
    | Thunk zz  -> zz ()
    | xs        -> xs

    let rec mplus xs ys =
      match xs with
      | Nil           -> force ys
      | Cons (x, xs)  -> cons x (from_fun @@ fun () -> mplus (force ys) xs)
      | Thunk   _     -> from_fun (fun () -> mplus (force ys) xs)
      | Waiting ss    ->
        let ys = force ys in
        (* handling waiting streams is tricky *)
        match unwrap_suspended ss, ys with
        (* if [xs] has no ready streams and [ys] is also a waiting stream then we merge them  *)
        | Waiting ss, Waiting ss' -> Waiting (ss @ ss')
        (* if [xs] has no ready streams but [ys] is not a waiting stream then we swap them,
           pushing waiting stream to the back of the new stream *)
        | Waiting ss, _           -> mplus ys @@ from_fun (fun () -> xs)
        (* if [xs] has ready streams then [xs'] contains some lazy stream that is ready to produce new answers *)
        | xs', _ -> mplus xs' ys

    and unwrap_suspended ss =
      let rec find_ready prefix = function
        | ({is_ready; zz} as s)::ss ->
          if is_ready ()
          then Some (from_fun zz), (List.rev prefix) @ ss
          else find_ready (s::prefix) ss
        | [] -> None, List.rev prefix
      in
      match find_ready [] ss with
        | Some s, [] -> s
        | Some s, ss -> mplus (force s) @@ Waiting ss
        | None , ss  -> Waiting ss

    let rec bind s f =
      match s with
      | Nil           -> Nil
      | Cons (x, s)   -> mplus (f x) (from_fun (fun () -> bind (force s) f))
      | Thunk zz      -> from_fun (fun () -> bind (zz ()) f)
      | Waiting ss    ->
        match unwrap_suspended ss with
        | Waiting ss ->
          let helper {zz} as s = {s with zz = fun () -> bind (zz ()) f} in
          Waiting (List.map helper ss)
        | s          -> bind s f

    let rec msplit = function
    | Nil           -> None
    | Cons (x, xs)  -> Some (x, xs)
    | Thunk zz      -> msplit @@ zz ()
    | Waiting ss    ->
      match unwrap_suspended ss with
      | Waiting _ -> None
      | xs        -> msplit xs

    let is_empty s =
      match msplit s with
      | Some _  -> false
      | None    -> true

    let rec map f = function
    | Nil          -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
    | Thunk zzz    -> from_fun (fun () -> map f @@ zzz ())
    | Waiting ss   ->
      let helper {zz} as s = {s with zz = fun () -> map f (zz ())} in
      Waiting (List.map helper ss)

    let rec iter f s =
      match msplit s with
      | Some (x, s) -> f x; iter f s
      | None        -> ()

    let rec filter p s =
      match msplit s with
      | Some (x, s) -> let s = filter p s in if p x then Cons (x, s) else s
      | None        -> Nil

    let rec fold f acc s =
      match msplit s with
      | Some (x, s) -> fold f (f acc x) s
      | None        -> acc

    let rec zip xs ys =
      match msplit xs, msplit ys with
      | None,         None          -> Nil
      | Some (x, xs), Some (y, ys)  -> Cons ((x, y), zip xs ys)
      | _                           -> invalid_arg "OCanren fatal (Stream.zip): streams have different lengths"

    let hd s =
      match msplit s with
      | Some (x, _) -> x
      | None        -> invalid_arg "OCanren fatal (Stream.hd): empty stream"

    let tl s =
      match msplit s with
      | Some (_, xs) -> xs
      | None         -> Nil

    let rec retrieve ?(n=(-1)) s =
      if n = 0
      then [], s
      else match msplit s with
      | None          -> [], Nil
      | Some (x, s)  -> let xs, s = retrieve ~n:(n-1) s in x::xs, s

    let take ?n s = fst @@ retrieve ?n s

  end
;;

@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

let logic = {logic with
  gcata = ();
  plugins =
    object
      method gmap      = logic.plugins#gmap
      method html      = logic.plugins#html
      method eq        = logic.plugins#eq
      method compare   = logic.plugins#compare
      method foldl     = logic.plugins#foldl
      method foldr     = logic.plugins#foldr
      method show fa x =
        GT.transform(logic)
          (GT.lift fa)
          (object inherit ['a] @logic[show]
             method c_Var _ s i cs =
               let c = match cs with
               | [] -> ""
               | _  -> sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
               in
               sprintf "_.%d%s" i c
             method c_Value _ _ x = x.GT.fx ()
           end)
          ()
          x
    end
}

module Var =
  struct
    type env    = int
    type scope  = int
    type anchor = int list

    let tabling_env = -1

    let unused_index = -1

    let non_local_scope = -6

    let new_scope =
      let scope = ref 0 in
      fun () -> (incr scope; !scope)

    let global_anchor = [-8]

    type t = {
      anchor        : anchor;
      env           : env;
      index         : int;
      mutable subst : Obj.t option;
      scope         : scope;
      constraints   : Obj.t list
    }

    let make ~env ~scope index = {
      env         = env;
      anchor      = global_anchor;
      subst       = None;
      constraints = [];
      index;
      scope;
    }

    let dummy =
      let env   = 0 in
      let scope = 0 in
      make ~env ~scope 0

    let valid_anchor anchor =
      anchor == global_anchor

    let reify r {index; constraints} =
      (index, List.map (fun x -> r @@ Obj.obj x) constraints)

    let equal x y =
      (x.index = y.index) && (x.env = y.env)

    let compare x y =
      if x.index <> y.index then x.index - y.index else x.env - y.env

    let hash x = Hashtbl.hash (x.env, x.index)
  end

module VarSet = Set.Make(Var)
module VarTbl = Hashtbl.Make(Var)

module VarMap =
  struct
    include Map.Make(Var)

    let update k f m =
      match f (try Some (find k m) with Not_found -> None) with
      | Some x -> add k x m
      | None   -> remove k m
  end

(* [Term] - encapsulates unsafe operations on OCaml's values extended with logic variables;
 *   provides set of functions to traverse these values
 *)
module Term :
  sig
    type t = Obj.t

    type value

    val repr : 'a -> t

    (* [var x] if [x] is logic variable returns it, otherwise returns [None] *)
    val var : 'a -> Var.t option

    (* [map ~fvar ~fval x] map over OCaml's value extended with logic variables;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
     *)
    val map : fvar:(Var.t -> t) -> fval:(value -> t) -> t -> t

    (* [iter ~fvar ~fval x] iteration over OCaml's value extended with logic variables;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
     *)
    val iter : fvar:(Var.t -> unit) -> fval:(value -> unit) -> t -> unit

    (* [fold ~fvar ~fval ~init x] fold over OCaml's value extended with logic variables;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
     *)
    val fold : fvar:('a -> Var.t -> 'a) -> fval:('a -> value -> 'a) -> init:'a -> t -> 'a

    exception Different_shape of int * int

    type label = L | R

    (* [fold ~fvar ~fval ~fvarval ~init x y] folds two OCaml's value extended with logic variables simultaneously;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
     *   if it finds logic variable in one term but regular value in another term in same place, it calls [fk];
     *   if two terms cannot be traversed simultaneously raises exception [Different_shape (tx, ty)],
     *   where [tx] and [ty] are Ocaml tags of disparate values
     *)
    val fold2 :
      fvar:('a -> Var.t -> Var.t -> 'a) ->
      fval:('a -> value -> value -> 'a)  ->
      fk:('a -> label -> Var.t -> t -> 'a) ->
      init:'a -> t -> t -> 'a

    val show : t -> string

    val equal   : t -> t -> bool
    val compare : t -> t -> int
    val hash    : t -> int
  end = struct
    type t = Obj.t
    type value = Obj.t

    let repr = Obj.repr

    let var_tag, var_size =
      let dummy = Obj.repr Var.dummy in
      Obj.tag dummy, Obj.size dummy

    let is_var tx sx x =
      if (tx = var_tag) && (sx = var_size) then
         let anchor = (Obj.obj x : Var.t).Var.anchor in
         (Obj.is_block @@ Obj.repr anchor) && (Var.valid_anchor anchor)
      else false

    let is_box t =
      if (t <= Obj.last_non_constant_constructor_tag) &&
         (t >= Obj.first_non_constant_constructor_tag)
      then true
      else false

    let is_int = (=) Obj.int_tag
    let is_str = (=) Obj.string_tag
    let is_dbl = (=) Obj.double_tag

    let is_valid_tag t = (is_int t) || (is_str t) || (is_dbl t)

    let is_valid_tag_exn t =
      if is_valid_tag t then () else failwith (sprintf "OCanren fatal: invalid value tag (%d)" t)

    let var x =
      let x = Obj.repr x in
      let tx = Obj.tag x in
      if is_box tx then
        let sx = Obj.size x in
        if is_var tx sx x then Some (Obj.magic x) else None
      else None

    let rec map ~fvar ~fval x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          fvar @@ Obj.magic x
        else
          let y = Obj.dup x in
          for i = 0 to sx - 1 do
            Obj.set_field y i @@ map ~fvar ~fval (Obj.field x i)
          done;
          y
      else begin
        is_valid_tag_exn tx;
        fval x
      end

    let rec iter ~fvar ~fval x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          fvar @@ Obj.magic x
        else
          for i = 0 to sx - 1 do
            iter ~fvar ~fval (Obj.field x i)
          done;
      else begin
        is_valid_tag_exn tx;
        fval x
      end

    let rec show x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          let v = Obj.magic x in
          match v.Var.constraints with
          | [] -> Printf.sprintf "_.%d" v.Var.index
          | cs -> Printf.sprintf "_.%d{=/= %s}" v.Var.index (String.concat "; " @@ List.map show cs)
        else
          let rec inner i =
            if i < sx then
              (show @@ Obj.field x i)::(inner (i+1))
            else []
          in
          Printf.sprintf "boxed %d <%s>" tx (String.concat ", " @@ inner 0)
      else begin
        is_valid_tag_exn tx;
        if tx = Obj.int_tag then
          Printf.sprintf "int<%d>" @@ Obj.magic x
        else if tx = Obj.string_tag then
          Printf.sprintf "string<%s>" @@ Obj.magic x
        else if tx = Obj.double_tag then
          Printf.sprintf "double<%e>" @@ Obj.magic x
        else assert false
      end

    let rec fold ~fvar ~fval ~init x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          fvar init @@ Obj.magic x
        else
          let rec inner i acc =
            if i < sx then
              let acc = fold ~fvar ~fval ~init:acc (Obj.field x i) in
              inner (i+1) acc
            else acc
          in
          inner 0 init
      else begin
        is_valid_tag_exn tx;
        fval init x
      end

    exception Different_shape of int * int

    type label = L | R

    let rec fold2 ~fvar ~fval ~fk ~init x y =
      let tx, ty = Obj.tag x, Obj.tag y in
      match is_box tx, is_box ty with
      | true, true -> begin
        let sx, sy = Obj.size x, Obj.size y in
        match is_var tx sx x, is_var ty sy y with
        | true, true    -> fvar init (Obj.magic x) (Obj.magic y)
        | true, false   -> fk init L (Obj.magic x) y
        | false, true   -> fk init R (Obj.magic y) x
        | false, false  ->
          if (tx = ty) && (sx = sy) then
            let fx, fy = Obj.field x, Obj.field y in
            let rec inner i acc =
              if i < sx then
                let acc = fold2 ~fvar ~fval ~fk ~init:acc (fx i) (fy i) in
                inner (i+1) acc
              else acc
            in
            inner 0 init
          else raise (Different_shape (tx, ty))
        end
      | true, false ->
        is_valid_tag_exn ty;
        let sx = Obj.size x in
        if is_var tx sx x then fk init L (Obj.magic x) y else raise (Different_shape (tx, ty))
      | false, true ->
        is_valid_tag_exn tx;
        let sy = Obj.size y in
        if is_var ty sy y then fk init R (Obj.magic y) x else raise (Different_shape (tx, ty))
      | false, false ->
        is_valid_tag_exn tx;
        is_valid_tag_exn ty;
        if tx = ty then
          fval init x y
        else raise (Different_shape (tx, ty))

    let rec equal x y =
      try
        fold2 x y ~init:true
          ~fvar:(fun acc v u ->
            acc &&
            (Var.equal v u) &&
            (List.length v.Var.constraints = List.length u.Var.constraints) &&
            (List.for_all2 equal v.Var.constraints u.Var.constraints)
          )
          ~fval:(fun acc x y -> acc && (x = y))
          ~fk:(fun _ _ _ _ -> false)
      with Different_shape _ -> false

    let compare' = compare

    let rec compare x y =
      try
        fold2 x y ~init:0
          ~fvar:(fun acc v u ->
            if acc <> 0 then acc
            else
              let acc = Var.compare v u in
              if acc <> 0 then acc
              else List.fold_left2 (fun acc x y -> if acc <> 0 then acc else compare x y) 0 v.Var.constraints u.Var.constraints
          )
          ~fval:(fun acc x y -> if acc <> 0 then acc else (compare' x y))
          ~fk:(fun _ _ _ _ -> -1)
      with Different_shape (tx, ty) -> compare' tx ty

    let rec hash x = fold x ~init:1
      ~fvar:(fun acc v -> Hashtbl.hash (Var.hash v, List.fold_left (fun acc x -> Hashtbl.hash (acc, hash x)) acc v.Var.constraints))
      ~fval:(fun acc x -> Hashtbl.hash (acc, Hashtbl.hash x))

  end

let (!!!) = Obj.magic

module Env :
  sig
    type t

    val empty         : unit -> t
    val create        : anchor:Var.env -> t
    val fresh         : scope:Var.scope -> t -> 'a
    val check         : t -> Var.t -> bool
    val check_exn     : t -> Var.t -> unit
    val is_var        : t -> 'a -> bool
    val var           : t -> 'a -> Var.t option
    val freevars      : t -> 'a -> VarSet.t
    val is_open       : t -> 'a -> bool
    val equal         : t -> t -> bool
  end =
  struct
    type t = {anchor : Var.env; mutable next : int}

    let last_anchor = ref 11
    let first_var = 10

    let empty () =
      incr last_anchor;
      {anchor = !last_anchor; next = first_var}

    let create ~anchor = {anchor; next = first_var}

    let fresh ~scope e =
      let v = !!!(Var.make ~env:e.anchor ~scope e.next) in
      e.next <- 1 + e.next;
      !!!v

    let check env v = (v.Var.env = env.anchor)

    let check_exn env v =
      if check env v then () else failwith "OCanren fatal (Env.check): wrong environment"

    let var env x =
      match Term.var x with
      | (Some v) as res -> check_exn env v; res
      | None            -> None

    let is_var env x = (var env x) <> None

    let freevars env x =
      Term.fold (Term.repr x) ~init:VarSet.empty
        ~fvar:(fun acc v -> VarSet.add v acc)
        ~fval:(fun acc _ -> acc)

    exception Open_Term

    let is_open env x =
      try
        Term.iter (Term.repr x)
          ~fvar:(fun _ -> raise Open_Term)
          ~fval:(fun _ -> ());
        false
      with Open_Term -> true

    let equal {anchor=a1} {anchor=a2} = (a1 = a2)
  end

module Binding :
  sig
    type t =
      { var   : Var.t
      ; term  : Term.t
      }

    val is_relevant : Env.t -> VarSet.t -> t -> bool

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end =
  struct
    type t =
      { var   : Var.t
      ; term  : Term.t
      }

    let is_relevant env vs {var; term} =
      (VarSet.mem var vs) ||
      (match Env.var env term with Some v -> VarSet.mem v vs | None -> false)

    let equal {var=v; term=t} {var=u; term=p} =
      (Var.equal v u) || (Term.equal t p)

    let compare {var=v; term=t} {var=u; term=p} =
      let res = Var.compare v u in
      if res <> 0 then res else Term.compare t p

    let hash {var; term} = Hashtbl.hash (Var.hash var, Term.hash term)
  end

module Subst :
  sig
    type t

    val empty : t

    val of_list : Binding.t list -> t
    val of_map  : Term.t VarMap.t -> t

    val split : t -> Binding.t list

    (* [apply env subst x] - applies [subst] to term [x],
     *   i.e. replaces every variable to relevant binding in [subst];
     *)
    val apply : Env.t -> t -> 'a -> 'a

    (* [is_bound x subst] - checks whether [x] is bound by [subst] *)
    val is_bound : Var.t -> t -> bool

    (* [freevars env subst x] - returns all free-variables of term [x] *)
    val freevars : Env.t -> t -> 'a -> VarSet.t

    (* [unify ~subsume ~scope env subst x y] performs unification of two terms [x] and [y] in [subst].
     *   Unification is a process of finding substituion [s] s.t. [s(x) = s(y)].
     *   Returns [None] if two terms are not unifiable.
     *   Otherwise it returns a pair of diff and new substituion.
     *   Diff is a list of pairs (var, term) that were added to the original substituion.
     *
     *   If [subsume] argument is passed and [true] then substituion binds variables only from left term,
     *   (i.e. it returns [s] s.t. [s(x) = y]).
     *   This can be used to perform subsumption check:
     *   [y] is subsumed by [x] (i.e. [x] is more general than [x]) if such a unification succeeds.
     *)
    val unify : ?subsume:bool -> ?scope:Var.scope -> Env.t -> t -> 'a -> 'a -> (Binding.t list * t) option

    val merge_disjoint : Env.t -> t -> t -> t

    (* [merge env s1 s2] merges two substituions *)
    val merge : Env.t -> t -> t -> t option

    (* [subsumed env s1 s2] checks that [s1] is subsumed by [s2] (i.e. [s2] is more general than [s1]).
     *   Subsumption relation forms a partial order on the set of substitutions.
     *)
    val subsumed : Env.t -> t -> t -> bool

    module Answer :
      sig
        type t = Term.t

        (* [subsumed env x y] checks that [x] is subsumed by [y] (i.e. [y] is more general than [x]) *)
        val subsumed : Env.t -> t -> t -> bool
      end

    val reify : Env.t -> t -> 'a -> Answer.t
  end =
  struct
    type t = Term.t VarMap.t

    let empty = VarMap.empty

    let of_list =
      ListLabels.fold_left ~init:empty ~f:(let open Binding in fun subst {var; term} ->
        if not @@ VarMap.mem var subst then
          VarMap.add var term subst
        else
          invalid_arg "OCanren fatal (Subst.of_list): invalid substituion"
      )

    let of_map m = m

    let split s = VarMap.fold (fun var term xs -> Binding.({var; term})::xs) s []

    type lterm = Var of Var.t | Value of Term.t

    let rec walk env subst x =
      (* walk var *)
      let rec walkv env subst v =
        Env.check_exn env v;
        match v.Var.subst with
        | Some term -> walkt env subst !!!term
        | None ->
            try walkt env subst (VarMap.find v subst)
            with Not_found -> Var v
      (* walk term *)
      and walkt env subst t =
        match Env.var env t with
        | Some v -> walkv env subst v
        | None   -> Value t
      in
      walkv env subst x

    (* same as [Term.map] but performs [walk] on the road *)
    let map ~fvar ~fval env subst x =
      let rec deepfvar v =
        Env.check_exn env v;
        match walk env subst v with
        | Var v   -> fvar v
        | Value x -> Term.map x ~fval ~fvar:deepfvar
      in
      Term.map x ~fval ~fvar:deepfvar

    (* same as [Term.iter] but performs [walk] on the road *)
    let iter ~fvar ~fval env subst x =
      let rec deepfvar v =
        Env.check_exn env v;
        match walk env subst v with
        | Var v   -> fvar v
        | Value x -> Term.iter x ~fval ~fvar:deepfvar
      in
      Term.iter x ~fval ~fvar:deepfvar

    (* same as [Term.fold] but performs [walk] on the road *)
    let fold ~fvar ~fval ~init env subst x =
      let rec deepfvar acc v =
        Env.check_exn env v;
        match walk env subst v with
        | Var v   -> fvar acc v
        | Value x -> Term.fold x ~fval ~fvar:deepfvar ~init:acc
      in
      Term.fold x ~init ~fval ~fvar:deepfvar

    exception Occurs_check

    let rec occurs env subst var term =
      iter env subst term
        ~fvar:(fun v -> if Var.equal v var then raise Occurs_check)
        ~fval:(fun x -> ())

    let extend ~scope env subst var term  =
      (* if occurs env subst var term then raise Occurs_check *)
      occurs env subst var term;
        (* assert (Env.var env var <> Env.var env term); *)

      (* It is safe to modify variables destructively if the case of scopes match.
       * There are two cases:
       * 1) If we do unification just after a conde, then the scope is already incremented and nothing goes into
       *    the fresh variables.
       * 2) If we do unification after a fresh, then in case of failure it doesn't matter if
       *    the variable is be distructively substituted: we will not look on it in future.
       *)
      if (scope = var.scope) && (scope <> Var.non_local_scope)
      then begin
        var.subst <- Some (Obj.repr term);
        subst
      end
        else
          VarMap.add var (Term.repr term) subst

    exception Unification_failed

    let unify ?(subsume=false) ?(scope=Var.non_local_scope) env subst x y =
      (* The idea is to do the unification and collect the unification prefix during the process *)
      let extend var term (prefix, subst) =
        let subst = extend ~scope env subst var term in
        (Binding.({var; term})::prefix, subst)
      in
      let rec helper x y acc =
        let open Term in
        fold2 x y ~init:acc
          ~fvar:(fun ((_, subst) as acc) x y ->
            match walk env subst x, walk env subst y with
            | Var x, Var y      ->
              if Var.equal x y then acc else extend x (Term.repr y) acc
            | Var x, Value y    -> extend x y acc
            | Value x, Var y    -> extend y x acc
            | Value x, Value y  -> helper x y acc
          )
          ~fval:(fun acc x y ->
              if x = y then acc else raise Unification_failed
          )
          ~fk:(fun ((_, subst) as acc) l v y ->
              if subsume && (l = Term.R)
              then raise Unification_failed
              else match walk env subst v with
              | Var v    -> extend v y acc
              | Value x  -> helper x y acc
          )
      in
      try
        let x, y = Term.(repr x, repr y) in
        Some (helper x y ([], subst))
      with Term.Different_shape _ | Unification_failed | Occurs_check -> None

    let apply env subst x = Obj.magic @@
      map env subst (Term.repr x)
        ~fvar:(fun v -> Term.repr v)
        ~fval:(fun x -> Term.repr x)

    let freevars env subst x =
      Env.freevars env @@ apply env subst x

    let is_bound = VarMap.mem

    let merge env subst1 subst2 = VarMap.fold (fun var term -> function
      | Some s  -> begin
        match unify env s !!!var term with
        | Some (_, s') -> Some s'
        | None         -> None
        end
      | None    -> None
    ) subst1 (Some subst2)

    let merge_disjoint env =
      VarMap.union (fun _ _ ->
        invalid_arg "OCanren fatal (Subst.merge_disjoint): substitutions intersect"
      )

    let subsumed env subst =
      VarMap.for_all (fun var term ->
        match unify env subst !!!var term with
        | Some ([], _)  -> true
        | _             -> false
      )

    module Answer =
      struct
        type t = Term.t

        let subsumed env x y =
          match unify ~subsume:true env empty y x with
          | Some _ -> true
          | None   -> false
      end

    let reify env subst x =
      map env subst (Term.repr x)
        ~fvar:(fun v -> Term.repr v)
        ~fval:(fun x -> Term.repr x)

  end

module Disequality :
  sig
    type t

    (* [empty] empty disequality constraint store *)
    val empty  : t

    (* [add env subst diseq x y] adds constraint [x =/= y] into disequality constraint store *)
    val add : Env.t -> Subst.t -> t -> 'a -> 'a -> t option

    (* [recheck env subst diseq bindings] - checks that disequality is not violated in refined substitution.
     *   [bindings] is a substitution prefix, i.e. new bindings obtained during unification.
     *   This function may rebuild internal representation of constraints and thus it returns new object.
     *   If constraint is violated then [None] is returned.
     *)
    val recheck : Env.t -> Subst.t -> t -> Binding.t list -> t option

    (* [project env subst diseq fv] - projects [diseq] into the set of free-variables [fv],
     *   i.e. it extracts only those constraints that are relevant to variables from [fv]
     *)
    val project : Env.t -> Subst.t -> t -> VarSet.t -> t

    (* [merge_disjoint env subst diseq diseq'] merges two disequality constraints *)
    val merge_disjoint : Env.t -> Subst.t -> t -> t -> t

    module Answer :
      sig
        (* [Answer.t] result of reification of disequality constraints *)
        type t

        (* [extract a v] returns list of `forbidden` terms for variable [v] *)
        val extract : t -> Var.t -> Term.t list

        val subsumed : Env.t -> t -> t -> bool
      end

    val reify : Env.t -> Subst.t -> t -> 'a -> Answer.t list
  end =
  struct
    module Answer =
      struct
        module S = Set.Make(Term)

        (* answer is a conjunction of single disequalities, i.g. (x =/= 1 /\ y =/= 2);
         * in order to efficiently extract disequalities relevant to particular variable we use map
         *)
        type t = S.t VarMap.t

        let empty = VarMap.empty

        let add env t var term =
          try
            let terms = S.add term @@ VarMap.find var t in
            VarMap.add var terms @@ VarMap.remove var t
          with Not_found ->
            VarMap.add var (S.singleton term) t

        let mem env t var term =
          try
            S.mem term @@ VarMap.find var t
          with Not_found -> false

        let extract t v =
          try S.elements @@ VarMap.find v t with Not_found -> []

        let subsumed env t t' =
          (* we should check that for each binding from [t'] there is
           * a binding in [t] that subsumes it;
           * Examples:
           *    (x =/= _.0) <= (x =/= 1 /\ x =/= 2), but
           *    (x =/= _.0) and (x =/= 1 /\ y =/= 2) are not ordered
           *)
          VarMap.for_all (fun var terms' ->
            try
              let terms = VarMap.find var t in
              S.for_all (fun term' ->
                S.exists (fun term ->
                  Subst.Answer.subsumed env term term'
                ) terms
              ) terms'
            with Not_found -> false
          ) t'
      end

    exception Disequality_violated
    exception Disequality_fulfilled

    (* Disequality constraints are represented as formula in CNF
     * where each atom is single disequality
     * (i.g. ({x =/= t} \/ {y =/= u}) /\ ({y =/= v} \/ {z =/= w}))
     *
     * Optimisation:
     * For each disjunct in the formula we choose one `sample` (i.e single disequality {x =/= t}).
     * Whenever we want to check the whole disequality constraint we
     * can check single `sample` from each conjunct.
     * If `sample` check is passed (i.e {x =/= t} holds in current substitution) we can
     * skip checks of other disjuncts in this conjunct.
     * Also note that after unification of two terms we can
     * check only those disequalities that involves changed variables.
     * Because of that we maintain an index - a map from variable index to
     * list of conjuncts for which this variable is a `sample`.
     * When `sample` check fails, we change index.
     * We choose another `sample` {y =/= u} and add it to the map for variable {y}.
     * There is no need to check previous samples in the future (because its assumption is already broken in current substitution)
    *)

    module Disjunct :
      sig
        (* Disjunction.t is a set of single disequalities joint by disjunction *)
        type t

        (* [make env subst x y] creates new disjunct from the disequality [x =/= y] *)
        val make : Env.t -> Subst.t -> 'a -> 'a -> t

        (* [sample disj] returns an index of variable involved in some disequality inside disjunction *)
        val samplevar : t -> Var.t

        (* [recheck env subst disj] - checks that disjunction of disequalities is
         *   not violated in (current) substitution.
         *   This function is designed to incrementally refine disequalities
         *   with a series of more and more specialized substitutions.
         *   If arbitary substitutions are passed the result may be invalid.
         *)
        val recheck : Env.t -> Subst.t -> t -> t

        val is_relevant : Env.t -> Subst.t -> t -> VarSet.t -> bool

        val freevars : Env.t -> Subst.t -> t -> VarSet.t

        val subsumed : Env.t -> Subst.t -> t -> t -> bool

        val simplify : Env.t -> Subst.t -> t -> t option

        val reify : Env.t -> Subst.t -> t -> Binding.t list
      end =
      struct
        type t = Term.t VarMap.t

        let update t =
          ListLabels.fold_left ~init:t
            ~f:(let open Binding in fun acc {var; term} ->
              if VarMap.mem var acc then
                (* in this case we have subformula of the form (x =/= t1) \/ (x =/= t2) which is always SAT *)
                raise Disequality_fulfilled
              else
                VarMap.add var term acc
            )

        let of_list = update VarMap.empty

        let samplevar t = fst @@ VarMap.max_binding t

        type status =
          | Fulfiled
          | Violated
          | Refined of Binding.t list

        let refine env subst x y =
          match Subst.unify env subst x y with
          | None              -> Fulfiled
          | Some ([], _)      -> Violated
          | Some (prefix, _)  -> Refined prefix

        let make env subst x y =
          match refine env subst x y with
          | Refined delta -> of_list delta
          | Fulfiled      -> raise Disequality_fulfilled
          | Violated      -> raise Disequality_violated

        let rec recheck env subst t =
          let var, term = VarMap.max_binding t in
          let unchecked = VarMap.remove var t in
          match refine env subst !!!var term with
          | Fulfiled       -> raise Disequality_fulfilled
          | Refined delta  -> update unchecked delta
          | Violated       ->
            if VarMap.is_empty unchecked then
              raise Disequality_violated
            else
              recheck env subst unchecked

        let simplify env subst ds =
          try
            let result = VarMap.fold (fun var term acc ->
              match refine env subst !!!var term with
              | Fulfiled       -> raise Disequality_fulfilled
              | Violated       -> acc
              | Refined delta  -> delta @ acc
            ) ds []
            in
            (* We should not get empty substituion delta here,
             * because it would mean that disequality is violated.
             * But we had to detect violations during search via `check`.
             *)
            assert (match result with [] -> false | _ -> true);
            Some (of_list result)
          with Disequality_fulfilled -> None

        let reify env subst t =
          VarMap.fold (fun var term xs -> Binding.({var; term})::xs) t []

        let is_relevant env subst t fv =
          (* left those disjuncts that contains binding only for variables from [fv],
           * otherwise it's possible to pick binding (x =/= t) from disjunct for
           * variable [x] that is not in [fv],
           * assign [t'] ([t =/= t']) to [x] and thus fulfill the disequality
           *)
           VarMap.for_all (fun var term ->
             (VarSet.mem var fv) ||
             (match Env.var env term with Some u -> VarSet.mem u fv | None -> false)
           ) t

        let freevars env subst t =
          VarMap.fold (fun _ term acc ->
            VarSet.union acc @@ Subst.freevars env subst term
          ) t VarSet.empty

        let subsumed env subst t t' =
          Subst.(subsumed env (of_map t') (of_map t))

      end

    module Conjunct :
      sig
        type t

        val empty : t

        val is_empty : t -> bool

        val make : Env.t -> Subst.t -> 'a -> 'a -> t

        val split : t -> t VarMap.t

        val recheck : Env.t -> Subst.t -> t -> t

        val project : Env.t -> Subst.t -> t -> VarSet.t -> t

        val merge_disjoint : Env.t -> Subst.t -> t -> t -> t

        (* [diff env subst t' t] computes diff of two conjuncts, that is [t' \ t].
         *   Returns a pair where first element is conjunct
         *   of constraints that were changed (i.e. refined) in [t'],
         *   and the second element is conjunct of entirely new constraints in [t'],
         *   that absent in [t].
         *)
        val diff : Env.t -> Subst.t -> t -> t -> t * t

        val reify : Env.t -> Subst.t -> t -> 'a -> Answer.t list
      end = struct
        let next_id = ref 0

        module M = Map.Make(struct type t = int let compare = (-) end)

        type t = Disjunct.t M.t

        let empty = M.empty

        let is_empty = M.is_empty

        let make env subst x y =
          let id = !next_id in
          next_id := !next_id + 1;
          M.singleton id @@ Disjunct.make env subst x y

        let split t =
          M.fold (fun id disj acc ->
            let var = Disjunct.samplevar disj in
            let upd = function
            | Some conj -> Some (M.add id disj conj)
            | None      -> Some (M.singleton id disj)
            in
            VarMap.update var upd acc
          ) t VarMap.empty

        let recheck env subst t =
          M.fold (fun id disj acc ->
              try
                M.add id (Disjunct.recheck env subst disj) acc
              with Disequality_fulfilled -> acc
          ) t M.empty

        let merge_disjoint env subst =
          M.union (fun _ _ _ ->
            invalid_arg "OCanren fatal (Conjunct.merge_disjoint): conjuncts intersect"
          )

        let diff env subst t' t =
          M.fold (fun id disj' (refined, added) ->
            try
              let disj = M.find id t in
              (* refined constraint should be more specialized (i.e. subsumed by earlier constraint) *)
              assert (Disjunct.subsumed env subst disj' disj);
              (M.add id disj' refined, added)
            with Not_found ->
              (refined, M.add id disj' added)
          ) t' (M.empty, M.empty)

        let remove_subsumed env subst cs =
          M.fold (fun id disj acc ->
            if M.exists (fun _ disj' -> Disjunct.subsumed env subst disj' disj) acc then
              (* if new disjunct subsumes some another then we don't add it;
               * that's because we have conjunction of disjuncts and we can keep only
               * the most specialized disjuncts
               *)
              acc
            else
              (* otherwise we should remove all disjuncts that subsume the newly added disjunct *)
              let acc = M.filter (fun _ disj' -> not (Disjunct.subsumed env subst disj disj')) acc in
              M.add id disj acc
          ) cs M.empty

        let project env subst t fv =
          let rec helper fv =
            let fv', t' = M.fold (fun id disj ((fv', conj) as acc) ->
              (* left those disjuncts that contain bindings only for variables from [fv],
               * and obtain a set of free variables from terms mentioned in those disjuncts
               *)
              if Disjunct.is_relevant env subst disj fv then
                let fv' = VarSet.union fv' @@ Disjunct.freevars env subst disj in
                fv', M.add id disj conj
              else
                fv', conj
            ) t (fv, M.empty)
            in
            if VarSet.equal fv fv' then t' else helper fv'
          in
          remove_subsumed env subst @@ helper fv

        let reify env subst t x =
          let t = M.fold (fun id disj acc ->
            match Disjunct.simplify env subst disj with
            | Some disj -> M.add id disj acc
            | None      -> acc
          ) t M.empty
          in
          let fv = Subst.freevars env subst x in
          let t = project env subst t fv in
          (* here we convert disequality in CNF form into DNF form;
           * we maintain a list of answers, that is a mapping [var -> term list] ---
           * list of disequality terms (without duplicates) for each variable
           *)
          M.fold (fun _ disj acc ->
            let bs = Disjunct.reify env subst disj in
              (* for each answer we append every atom in disjunct to it,
               * obtaining a list of new `extended` answers;
               * then we `concat` these lists into single list
               *)
            ListLabels.map acc ~f:(fun answ ->
                let open Binding in
                (* it might be the case that some atom in the disjunct
                 * is a duplicate of some other disequality in the answer;
                 * in this case we can throw away the whole disjunct (and keep only original answer)
                 * because it would not produce new extended answers;
                 * i.g. answer is [(x =/= 1) /\ (y =/= 2)] and the disjunct is [(x =/= 1) \/ (z =/= 3)],
                 * then extended answers are [(x =/= 1) /\ (y =/= 2)] and [(x =/= 1) /\ (y =/= 2) /\ (z =/= 3)],
                 * but the second one is subsumed by the first one and can be thrown away
                 *)
                if List.exists (fun {var; term} -> Answer.mem env answ var term) bs then
                  [answ]
                else
                  List.map (fun {var; term} -> Answer.add env answ var term) bs
              ) |> List.concat
          ) t [Answer.empty]

      end

    type t = Conjunct.t VarMap.t

    let empty = VarMap.empty

    (* merges all conjuncts (linked to different variables) into one *)
    let combine env subst cstore =
      VarMap.fold (fun _ -> Conjunct.merge_disjoint env subst) cstore Conjunct.empty

    let merge_disjoint env subst = VarMap.union (fun _ c1 c2 ->
      let c = Conjunct.merge_disjoint env subst c1 c2 in
      if Conjunct.is_empty c then None else Some c
    )

    let update env subst conj = merge_disjoint env subst (Conjunct.split conj)

    let add env subst cstore x y =
      try
        Some (update env subst (Conjunct.make env subst x y) cstore)
      with
        | Disequality_fulfilled -> Some cstore
        | Disequality_violated  -> None

    let recheck env subst cstore bs =
      let helper var cstore =
        try
          let conj = VarMap.find var cstore in
          let cstore = VarMap.remove var cstore in
          update env subst (Conjunct.recheck env subst conj) cstore
        with Not_found -> cstore
      in
      try
        let cstore = ListLabels.fold_left bs ~init:cstore
          ~f:(let open Binding in fun cstore {var; term} ->
            let cstore = helper var cstore in
            match Env.var env term with
            | Some u -> helper u cstore
            | None   -> cstore
          )
        in
        Some cstore
      with Disequality_violated -> None

    let project env subst cstore fv =
      Conjunct.(split @@ project env subst (combine env subst cstore) fv)

    let reify env subst cstore x =
      Conjunct.reify env subst (combine env subst cstore) x

end

module Answer :
  sig
    (* [Answer.t] - a type that represents (untyped) answer to a query *)
    type t

    (* [make env t] creates the answer from the environment and term (with constrainted variables)  *)
    val make : Env.t -> Term.t -> t

    (* [lift env a] lifts the answer into different environment, replacing all variables consistently *)
    val lift : Env.t -> t -> t

    (* [env a] returns an environment of the answer *)
    val env : t -> Env.t

    (* [unctr_term a] returns a term with unconstrained variables *)
    val unctr_term : t -> Term.t

    (* [ctr_term a] returns a term with constrained variables *)
    val ctr_term : t -> Term.t

    (* [disequality a] returns all disequality constraints on variables in term as a list of bindings *)
    val disequality : t -> Binding.t list

    (* [equal t t'] syntactic equivalence (not an alpha-equivalence) *)
    val equal : t -> t -> bool

    (* [hash t] hashing that is consistent with syntactic equivalence *)
    val hash : t -> int
  end = struct
    type t = Env.t * Term.t

    let make env t = (env, t)

    let env (env, _) = env

    let unctr_term (_, t) =
      Term.map t
        ~fval:(fun x -> Term.repr x)
        ~fvar:(fun v -> Term.repr {v with Var.constraints = []})

    let ctr_term (_, t) = t

    let disequality (env, t) =
      let rec helper acc x =
        Term.fold x ~init:acc
          ~fval:(fun acc _ -> acc)
          ~fvar:(fun acc var ->
            ListLabels.fold_left var.Var.constraints ~init:acc
              ~f:(fun acc ctr_term ->
                let ctr_term = Term.repr ctr_term in
                let var = {var with Var.constraints = []} in
                let term = unctr_term @@ (env, ctr_term) in
                let acc = Binding.({var; term})::acc in
                helper acc ctr_term
              )
          )
      in
      helper [] t

    let lift env' (env, t) =
      let vartbl = VarTbl.create 31 in
      let rec helper x =
        Term.map x
          ~fval:(fun x -> Term.repr x)
          ~fvar:(fun v -> Term.repr @@
            try
              VarTbl.find vartbl v
            with Not_found ->
              let new_var = Env.fresh ~scope:Var.non_local_scope env' in
              VarTbl.add vartbl v new_var;
              {new_var with Var.constraints =
                List.map (fun x -> helper x) v.Var.constraints
                |> List.sort Term.compare
              }
          )
      in
      (env', helper t)

    let check_envs_exn env env' =
      if Env.equal env env' then () else
        failwith "OCanren fatal (Answer.check_envs): answers from different environments"

    let equal (env, t) (env', t') =
      check_envs_exn env env';
      Term.equal t t'

    let hash (env, t) = Term.hash t

  end

module State :
  sig
    type t =
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Disequality.t
      ; scope : Var.scope
      }

    val empty : unit -> t

    val fresh : t -> 'a

    val new_scope : t -> t

    val unify : 'a -> 'a -> t -> t option
    val diseq : 'a -> 'a -> t -> t option

    val reify : 'a -> t -> Answer.t list
  end = struct
    type t =
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Disequality.t
      ; scope : Var.scope
      }

    type reified = Env.t * Term.t

    let empty () =
      { env   = Env.empty ()
      ; subst = Subst.empty
      ; ctrs  = Disequality.empty
      ; scope = Var.new_scope ()
      }

    let env   {env} = env
    let subst {subst} = subst
    let constraints {ctrs} = ctrs
    let scope {scope} = scope

    let fresh {env; scope} = Env.fresh ~scope env

    let new_scope st = {st with scope = Var.new_scope ()}

    let unify x y ({env; subst; ctrs; scope} as st) =
        match Subst.unify ~scope env subst x y with
        | None -> None
        | Some (prefix, subst) ->
          match Disequality.recheck env subst ctrs prefix with
          | None      -> None
          | Some ctrs -> Some {st with subst; ctrs}

    let diseq x y ({env; subst; ctrs; scope} as st) =
      match Disequality.add env subst ctrs x y with
      | None      -> None
      | Some ctrs -> Some {st with ctrs}

    let reify x {env; subst; ctrs} =
      let answ = Subst.reify env subst x in
      let diseqs = Disequality.reify env subst ctrs x in
      if List.length diseqs = 0 then
        [Answer.make env answ]
      else
        ListLabels.map diseqs ~f:(fun diseq ->
          let rec helper forbidden t =
            Term.map t
              ~fval:(fun x -> Term.repr x)
              ~fvar:(fun v -> Term.repr @@
                if List.mem v.Var.index forbidden then v
                else
                  {v with Var.constraints =
                    Disequality.Answer.extract diseq v
                    |> List.filter (fun dt ->
                      match Env.var env dt with
                      | Some u  -> not (List.mem u.Var.index forbidden)
                      | None    -> true
                    )
                    |> List.map (fun x -> helper (v.Var.index::forbidden) x)
                    (* TODO: represent [Var.constraints] as [Set];
                     * TODO: hide all manipulations on [Var.t] inside [Var] module;
                     *)
                    |> List.sort Term.compare
                  }
              )
          in
          Answer.make env (helper [] answ)
      )

  end

type 'a goal' = State.t -> 'a

type goal = State.t Stream.t goal'

let success st = Stream.single st
let failure _  = Stream.nil

let (===) x y st =
  match State.unify x y st with
  | Some st -> success st
  | None    -> failure st

let (=/=) x y st =
  match State.diseq x y st with
  | Some st -> success st
  | None    -> failure st

let delay g st = Stream.from_fun (fun () -> g () st)

let conj f g st = Stream.bind (f st) g
let (&&&) = conj
let (?&) gs = List.fold_right (&&&) gs success

let disj_base f g st = Stream.mplus (f st) (Stream.from_fun (fun () -> g st))

let disj f g st = let st = State.new_scope st in disj_base f g |> (fun g -> Stream.from_fun (fun () -> g st))

let (|||) = disj

let (?|) gs st =
  let st = State.new_scope st in
  let rec inner = function
  | [g]   -> g
  | g::gs -> disj_base g (inner gs)
  | [] -> failwith "Wrong argument of (?!)"
  in
  inner gs |> (fun g -> Stream.from_fun (fun () -> g st))

let conde = (?|)

let call_fresh f st =
  let x = State.fresh st in
  f x st

module Fresh =
  struct
    let succ prev f = call_fresh (fun x -> prev (f x))

    let zero  f = f
    let one   f = succ zero f
    let two   f = succ one f

    (* N.B. Manual inlining of numerals will speed-up OCanren a bit (mainly because of less memory consumption) *)
    (* let two   g = fun st ->
      let scope = State.scope st in
      let env = State.env st in
      let q = Env.fresh ~scope env in
      let r = Env.fresh ~scope env in
      g q r st *)

    let three f = succ two f
    let four  f = succ three f
    let five  f = succ four f

    let q     = one
    let qr    = two
    let qrs   = three
    let qrst  = four
    let pqrst = five
  end

(* ******************************************************************************* *)
(* ************************** Reification stuff ********************************** *)

include (struct

type ('a, 'b) injected = 'a
type ('a, 'b) inj = ('a,'b) injected
type helper = Env.t
(* camlp5  doesn't support nonrec, so dirty hack *)
type 'a logic_ = 'a logic
type 'a logic = 'a logic_

include MiniKanrenTypes.Ts

let rec reify env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

let rec prjc of_int env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (prjc of_int env) v in of_int i cs
  | None   -> Obj.magic x

module Fmap0 (T : T0) =
  struct
    external distrib : T.t -> (T.t, T.t) injected = "%identity"

    let rec reify env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify env) v in Var (i, cs)
      | None   -> Value (T.fmap x)

    let rec prjc of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc of_int env) v in of_int i cs
      | None   -> T.fmap x
  end

module Fmap (T : T1) =
  struct
    external distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected = "%identity"

    let rec reify r env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r env) v in Var (i, cs)
      | None   -> Value (T.fmap (r env) x)

    let rec prjc r of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r of_int env) v in of_int i cs
      | None   -> T.fmap (r env) x
  end

module Fmap2 (T : T2) =
  struct
    external distrib : (('a,'b) injected, ('c, 'd) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected = "%identity"

    let rec reify r1 r2 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) x)

    let rec prjc r1 r2 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) x
  end

module Fmap3 (T : T3) =
  struct
    external distrib : (('a, 'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected = "%identity"

    let rec reify r1 r2 r3 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 r3 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) x)

    let rec prjc r1 r2 r3 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) (r3 env) x
end

module Fmap4 (T : T4) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                     (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x)

  let rec prjc r1 r2 r3 r4 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x
end

module Fmap5 (T : T5) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x)

  let rec prjc r1 r2 r3 r4 r5 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x
end

module Fmap6 (T : T6) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 r6 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 r6 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x)

  let rec prjc r1 r2 r3 r4 r5 r6 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 r6 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x
end

end : sig
  type ('a, 'b) injected

  val reify : Env.t -> ('a, 'a logic) injected -> 'a logic

  val prjc : (int -> 'a list -> 'a) -> Env.t -> ('a, 'a logic) injected -> 'a

  include module type of struct include MiniKanrenTypes.Ts end

  include MiniKanrenTypes.Fmaps with type ('a,'b) inj = ('a,'b) injected
                                 and type helper = Env.t
                                 and type 'a logic_ = 'a logic

end)

external lift : 'a -> ('a, 'a) injected                      = "%identity"
external inj  : ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"

exception Not_a_value

let to_logic x = Value x

let from_logic = function
| Value x    -> x
| Var (_, _) -> raise Not_a_value

let (!!) x = inj (lift x)

class type ['a,'b] reified = object
  method is_open : bool
  method prj     : 'a
  method reify   : (Env.t -> ('a, 'b) injected -> 'b) -> 'b
  method prjc    : (Env.t -> ('a, 'b) injected -> 'a) -> 'a
end

let make_rr : Env.t -> ('a, 'b) injected -> ('a, 'b) reified  = fun env x ->
  object (self)
    method is_open            = Env.is_open env x
    method prj                = if self#is_open then raise Not_a_value else !!!x
    method reify reifier      = reifier env x
    method prjc  onvar        = onvar   env x
  end

let prj x = let rr = make_rr (Env.empty ()) x in rr#prj

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module Curry =
  struct
    let one = (@@)
    let succ k f x = k (fun tup -> f (x, tup))
  end

module Uncurry =
  struct
    let one = (@@)
    let succ k f (x,y) = k (f x) y
  end

module LogicAdder :
  sig
    val zero : goal -> goal
    val succ : ('a -> State.t -> 'd) -> (('e, 'f) injected -> 'a) -> State.t -> ('e, 'f) injected * 'd
  end = struct
    let zero f      = f
    let succ prev f = call_fresh (fun logic st -> (logic, prev (f logic) st))
  end

module ReifyTuple = struct
  let one x env = make_rr env x
  let succ prev (x, xs) env = (make_rr env x, prev xs env)
end

let succ n () =
  let adder, app, ext, uncurr = n () in
  (LogicAdder.succ adder, ReifyTuple.succ app, ExtractDeepest.succ ext, Uncurry.succ uncurr)

let one   () = (LogicAdder.(succ zero)), ReifyTuple.one, ExtractDeepest.ext2, Uncurry.one
let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let qrstu = five

let run n g h =
  let adder, reifier, ext, uncurr = n () in
  let args, stream = ext @@ adder g @@ State.empty () in
  Stream.bind stream (fun st -> Stream.of_list @@ State.reify args st)
  |> Stream.map (fun answ ->
    uncurr h @@ reifier (Obj.magic @@ Answer.ctr_term answ) (Answer.env answ)
  )


(** ************************************************************************* *)
(** Tabling primitives                                                        *)

module Table :
  sig
    (* Type of table.
     * Table is a map from answer term to the set of answer terms,
     * i.e. Answer.t -> [Answer.t]
     *)
    type t

    val create   : unit -> t

    val call : t -> ('a -> goal) -> 'a -> goal
  end = struct

    module H = Hashtbl.Make(Answer)

    module Cache :
      sig
        type t

        val create    : unit -> t

        val add       : t -> Answer.t -> unit
        val contains  : t -> Answer.t -> bool
        val consume   : t -> 'a -> goal
      end =
      struct
        (* Cache is a pair of queue-like list of answers plus hashtbl of answers;
         * Queue is used because new answers may arrive during the search,
         * we store this new answers to the end of the queue while read from the beginning.
         * Hashtbl is used for a quick check that new added answer is not already contained in the cache.
         *)
        type t = Answer.t list ref * unit H.t

        let create () = (ref [], H.create 11)

        let add (cache, tbl) answ =
          cache := List.cons answ !cache;
          H.add tbl answ ()

        let contains (_, tbl) answ =
          try
            H.find tbl answ;
            true
          with Not_found -> false

        let consume (cache, _) args =
          let open State in fun {env; subst; scope} as st ->
          let st = State.new_scope st in
          (* [helper iter seen] consumes answer terms from cache one by one
           *   until [iter] (i.e. current pointer into cache list) is not equal to [seen]
           *   (i.e. to the head of seen part of the cache list)
           *)
          let rec helper iter seen =
            if iter == seen then
              (* update `seen` - pointer to already seen part of cache *)
              let seen = !cache in
              (* delayed check that current head of cache is not equal to head of seen part *)
              let is_ready () = seen != !cache  in
              (* delayed thunk starts to consume unseen part of cache  *)
              Stream.suspend ~is_ready @@ fun () -> helper !cache seen
            else
              (* consume one answer term from cache and `lift` it to the current environment *)
              let answ, tail = (Answer.lift env @@ List.hd iter), List.tl iter in
              match State.unify (Obj.repr args) (Answer.unctr_term answ) st with
                | None -> helper tail seen
                | Some ({subst=subst'; ctrs=ctrs'} as st') ->
                  begin
                  (* check `answ` disequalities against external substitution *)
                  let ctrs = ListLabels.fold_left (Answer.disequality answ) ~init:Disequality.empty
                    ~f:(let open Binding in fun acc {var; term} ->
                      match Disequality.add env Subst.empty acc (Term.repr var) term with
                      (* we should not violate disequalities *)
                      | None     -> assert false
                      | Some acc -> acc
                    )
                  in
                  match Disequality.recheck env subst' ctrs (Subst.split subst') with
                  | None      -> helper tail seen
                  | Some ctrs ->
                    let st' = {st' with ctrs = Disequality.merge_disjoint env subst' ctrs' ctrs} in
                    Stream.(cons st' (from_fun @@ fun () -> helper tail seen))
                  end
          in
          helper !cache []

      end

    type t = Cache.t H.t

    let make_answ args st =
      let env = Env.create ~anchor:Var.tabling_env in
      let [answ] = State.reify args st in
      Answer.lift env answ

    let create () = H.create 1031

    let call tbl g args = let open State in fun ({env; subst; ctrs} as st) ->
      (* we abstract away disequality constraints before lookup in the table *)
      let abs_st = {st with ctrs = Disequality.empty} in
      let key = make_answ args abs_st in
      try
        (* slave call *)
        Cache.consume (H.find tbl key) args st
      with Not_found ->
        (* master call *)
        let cache = Cache.create () in
        H.add tbl key cache;
        (* auxiliary goal for addition of new answer to the cache  *)
        let hook ({env=env'; subst=subst'; ctrs=ctrs'} as st') =
          let answ = make_answ args st' in
          if not (Cache.contains cache answ) then begin
            Cache.add cache answ;
            (* TODO: we only need to check diff, i.e. [subst' \ subst] *)
            match Disequality.recheck env subst' ctrs (Subst.split subst') with
            | None      -> failure ()
            | Some ctrs ->
              success {st' with ctrs = Disequality.merge_disjoint env subst' ctrs ctrs'}
          end
          else failure ()
        in
        ((g args) &&& hook) abs_st
  end

module Tabling =
  struct
    let succ n () =
      let currier, uncurrier = n () in
      let sc = (Curry.succ : (('a -> 'b) -> 'c) -> ((((_, _) injected as 'k) * 'a -> 'b) -> 'k -> 'c)) in
      (sc currier, Uncurry.succ uncurrier)

    let one () = ((Curry.(one) : ((_, _) injected -> _) as 'x -> 'x), Uncurry.one)

    let two   () = succ one ()
    let three () = succ two ()
    let four  () = succ three ()
    let five  () = succ four ()

    let tabled n g =
      let tbl = Table.create () in
      let currier, uncurrier = n () in
      currier (Table.call tbl @@ uncurrier g)

    let tabledrec n g_norec =
      let tbl = Table.create () in
      let currier, uncurrier = n () in
      let g = ref (fun _ -> assert false) in
      let g_rec args = uncurrier (g_norec !g) args in
      let g_tabled = Table.call tbl g_rec in
      g := currier g_tabled;
      !g
end

(* Tracing/debugging stuff *)

(* module State

    let show  (env, subst, constr, scp) =
      sprintf "st {%s, %s} scope=%d" (Subst.show subst) (Constraints.show ~env constr) scp
*)

(* module Subst
    let show m =
      let b = Buffer.create 40 in
      Buffer.add_string b "subst {\n";
      M.iter (fun i {new_val} -> bprintf b "  %d -> %s;\n" i (generic_show new_val)) m;
      Buffer.add_string b "}";
      Buffer.contents b
=======


let unify ?p (x: _ injected) y =
  Trace.(trace two @@ unif ?p) x y (
    let open State in fun {env; subst; ctrs; scope} as st ->
    match Subst.unify env x y scope subst with
    | None -> failure ~reason:"Unification failed" st
    | Some (prefix, s) ->
        try
          let ctrs' = Constraints.check ~prefix env s ctrs in
          success {st with subst=s; ctrs=ctrs'}
        with Disequality_violated ->
          failure ~reason:"Disequality constraints violated" st
  )

let (===) x y = unify x y

let diseq ?p x y =
  Trace.(trace two @@ diseq ?p) x y (
    let open State in fun {env; subst; ctrs; scope} as st ->
    (* For disequalities we unify in non-local scope to prevent defiling *)
    match Subst.unify env x y non_local_scope subst with
    | None -> success st
    | Some ([],_) -> failure ~reason:"Constraint cannot be fulfilled" st
    | Some (prefix,_) ->
        let ctrs' = Constraints.extend ~prefix env ctrs in
        success {st with ctrs=ctrs'}
  )

let (=/=) x y = diseq x y

let delay : (unit -> goal) -> goal = fun g ->
  fun st -> MKStream.from_fun (fun () -> g () st)
>>>>>>> 81053c4... WIP: tabling

    let pretty_show is_var m =
      let b = Buffer.create 40 in
      bprintf b "subst {\n";
      M.iter (fun i {new_val} -> bprintf b "  %d -> %s;\n" i (pretty_generic_show is_var new_val)) m;
      bprintf b "}";
      Buffer.contents b
*)

(* module Constraints

  let bprintf_single ~env b cs =
    let rec helper = function
    | [] -> ()
    | c :: tl ->
          bprintf b "%d -> %s;" (!!!c.Subst.var : Var.t).index (pretty_generic_show (Env.is_var env) c.Subst.term);
          helper tl
    in
    helper cs

  let show_single ~env (c: single) =
    let b = Buffer.create 40 in
    bprintf b " ( ";
    let () = bprintf_single ~env b c in
    bprintf b " ) ";
    Buffer.contents b

  let show ~env cstore =
    let b = Buffer.create 40 in
    M.iter (fun k css ->
      bprintf b "\t%d -> [ " k;
      List.iter (fun s -> bprintf_single ~env b s) css;
      bprintf b " ]\n";
    ) cstore;
    Buffer.contents b

*)


(*
let generic_show ?(maxdepth=99999) x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner depth o =
    if depth > maxdepth then Buffer.add_string b "..." else
      match wrap o with
      | Invalid n                                         -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
      | Unboxed s when Obj.(string_tag = (tag @@ repr s)) -> bprintf b "\"%s\"" (!!!s)
      | Unboxed n when !!!n = 0                           -> Buffer.add_string b "[]"
      | Unboxed n                                         -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!!n))
      | Boxed  (t, l, f) ->
          Buffer.add_string b (Printf.sprintf "boxed %d <" t);
          for i = 0 to l - 1 do (inner (depth+1) (f i); if i<l-1 then Buffer.add_string b " ") done;
          Buffer.add_string b ">"
  in
  inner 0 x;
  Buffer.contents b

(* TODO *)
let pretty_generic_show ?(maxdepth= 99999) is_var x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner depth term =
    if depth > maxdepth then Buffer.add_string b "..." else
    if is_var !!!term then begin
      let var = (!!!term : Var.t) in
      match var.subst with
      | Some term ->
          bprintf b "{ _.%d with subst=" var.index;
          inner (depth+1) term;
          bprintf b " }"
      | None -> bprintf b "_.%d" var.index
    end else match wrap term with
      | Invalid n                                         -> bprintf b "<invalid %d>" n
      | Unboxed s when Obj.(string_tag = (tag @@ repr s)) -> bprintf b "\"%s\"" (!!!s)
      | Unboxed n when !!!n = 0                           -> Buffer.add_string b "[]"
      | Unboxed n                                         -> bprintf b  "int<%d>" (!!!n)
      | Boxed  (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (depth+1) (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner 0 x;
  Buffer.contents b

let trace msg g = fun state ->
  printf "%s: %s\n%!" msg (State.show state);
  g state

let reify_with_state (env,subs,cs,_) term = reify' env subs (Constraints.reify env subs cs) (Obj.repr term)

let project1 ~msg : (helper -> 'b -> string) -> ('a, 'b) injected -> goal = fun shower q st ->
  printf "%s %s\n%!" msg (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st q);
  success st

let project2 ~msg : (helper -> 'b -> string) -> (('a, 'b) injected as 'v) -> 'v -> goal = fun shower q r st ->
  printf "%s '%s' and '%s'\n%!" msg (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st q)
                                    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st r);
  success st

let project3 ~msg : (helper -> 'b -> string) -> (('a, 'b) injected as 'v) -> 'v -> 'v -> goal = fun shower q r s st ->
  printf "%s '%s' and '%s' and '%s'\n%!" msg
    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st q)
    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st r)
    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st s);
  success st

let unitrace ?loc shower x y = fun st ->
  incr logged_unif_counter;

  let ans = (x === y) st in
  (* printf "%d: unify '%s' and '%s'" !logged_unif_counter (shower (helper_of_state st) x) (shower (helper_of_state st) y);
  (match loc with Some l -> printf " on %s" l | None -> ());

  if Stream.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans

let diseqtrace shower x y = fun st ->
  incr logged_diseq_counter;
  let ans = (x =/= y) st in
  (* printf "%d: (=/=) '%s' and '%s'" !logged_diseq_counter
    (shower (helper_of_state st) x)
    (shower (helper_of_state st) y);
  if Stream.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans;;

(* ***************************** a la relational StdLib here ***************  *)

let report_counters () = ()
*)
