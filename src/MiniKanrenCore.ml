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

let (!!!) = Obj.magic

type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let is_valid_tag t =
  Obj.(
    not (List.mem t
      [lazy_tag    ; closure_tag; object_tag; infix_tag    ; forward_tag    ; no_scan_tag;
       abstract_tag; custom_tag ; custom_tag; unaligned_tag; out_of_heap_tag
      ])
  )

let rec wrap x =
  Obj.(
    let is_unboxed obj =
      is_int obj ||
      (fun t -> t = string_tag || t = double_tag) (tag obj)
    in
    if is_unboxed x
    then Unboxed x
    else
      let t = tag x in
      if is_valid_tag t
      then
        let f = if t = double_array_tag then !!! double_field else field in
        Boxed (t, size x, f x)
      else Invalid t
    )


module Stream =
  struct

    module Internal =
      struct
        type 'a t =
          | Nil
          | Thunk  of 'a thunk
          | Single of 'a
          | Choice of 'a * ('a t)
        and 'a thunk = unit -> 'a t

        let nil        = Nil
        let single x   = Single x
        let choice a f = Choice (a, f)
        let inc    f   = Thunk f
        let from_fun   = inc

        let rec is_empty = function
        | Nil      -> true
        | Thunk f  -> is_empty @@ f ()
        | _        -> false

        let force = function
        | Thunk f -> f ()
        | fs      -> fs

        let rec mplus fs gs =
          match fs with
          | Nil            -> force gs
          | Thunk   _      -> inc (fun () -> mplus (force gs) fs)
          | Single  a      -> choice a gs
          | Choice (a, hs) -> choice a (from_fun @@ fun () -> mplus (force gs) hs)

        let rec bind xs g =
          match xs with
          | Nil           -> Nil
          | Thunk   f     -> inc (fun () -> bind (f ()) g)
          | Single  c     -> g c
          | Choice (c, f) -> mplus (g c) (from_fun (fun () -> bind (force f) g))

        let rec of_list = function
          | []    -> Nil
          | x::[] -> Single x
          | x::xs -> Choice (x, of_list xs)

        let rec map f = function
          | Nil             -> Nil
          | Single x        -> Single (f x)
          | Choice (x, xs)  -> Choice (f x, map f xs)
          | Thunk thunk -> from_fun (fun () -> map f @@ thunk ())
      end

    type 'a internal = 'a Internal.t
    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec of_mkstream = function
    | Internal.Nil -> Nil
    | Internal.Thunk f -> from_fun (fun () -> of_mkstream @@ f ())
    | Internal.Single a -> Cons (a, Nil)
    | Internal.Choice (a, f) -> Cons (a, from_fun (fun () -> of_mkstream @@ Internal.force f))

    let rec is_empty = function
    | Nil    -> true
    | Lazy s -> is_empty @@ Lazy.force s
    | _      -> false

    let rec retrieve ?(n=(-1)) s =
      if n = 0
      then [], s
      else match s with
          | Nil          -> [], s
          | Cons (x, xs) -> let xs', s' = retrieve ~n:(n-1) xs in x::xs', s'
          | Lazy  z      -> retrieve ~n (Lazy.force z)

    let take ?(n=(-1)) s = fst @@ retrieve ~n s

    let hd s = List.hd @@ take ~n:1 s
    let tl s = snd @@ retrieve ~n:1 s

    let rec map f = function
    | Nil          -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
    | Lazy s       -> Lazy (Lazy.from_fun (fun () -> map f @@ Lazy.force s))

    let rec iter f = function
    | Nil          -> ()
    | Cons (x, xs) -> f x; iter f xs
    | Lazy s       -> iter f @@ Lazy.force s

    let rec zip fs gs =
      match (fs, gs) with
      | Nil         , Nil          -> Nil
      | Cons (x, xs), Cons (y, ys) -> Cons ((x, y), zip xs ys)
      | _           , Lazy s       -> Lazy (Lazy.from_fun (fun () -> zip fs (Lazy.force s)))
      | Lazy s      , _            -> Lazy (Lazy.from_fun (fun () -> zip (Lazy.force s) gs))
      | Nil, _      | _, Nil       -> failwith "OCanren fatal (Stream.zip): streams have different lengths"

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

    let compare x y =
      assert (x.env = y.env);
      x.index - y.index

  end

type ('a, 'b) injected = 'a

external lift : 'a -> ('a, 'a) injected                      = "%identity"
external inj  : ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"

module type T1 =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type T2 =
  sig
    type ('a, 'b) t
    val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end

module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
  end

module type T4 =
  sig
    type ('a, 'b, 'c, 'd) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
  end

module type T5 =
  sig
    type ('a, 'b, 'c, 'd, 'e) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
  end

module type T6 =
  sig
    type ('a, 'b, 'c, 'd, 'e, 'f) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

type helper = < isVar : 'a . 'a -> bool >

let to_var (c : helper) x r =
  if c#isVar x
  then
    let x : Var.t = !!!x in
    !!!(Var (x.index, List.map (!!!(r c)) x.Var.constraints))
  else failwith "OCanren fatal (to_var): not a logic variable"

module Fmap (T : T1) =
  struct
    external distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected = "%identity"

    let rec reify r (c : helper) x =
      if c#isVar x
      then to_var c x (reify r)
      else Value (T.fmap (r c) x)
  end

module Fmap2 (T : T2) =
  struct
    external distrib : (('a,'b) injected, ('c, 'd) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected = "%identity"

    let rec reify r1 r2 (c : helper) x =
      if c#isVar x
      then to_var c x (reify r1 r2)
      else Value (T.fmap (r1 c) (r2 c) x)
  end

module Fmap3 (T : T3) =
  struct
    external distrib : (('a, 'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected = "%identity"

    let rec reify r1 r2 r3 (c : helper) x =
      if c#isVar x then to_var c x (reify r1 r2 r3)
      else Value (T.fmap (r1 c) (r2 c) (r3 c) x)
end

module Fmap4 (T : T4) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                     (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 (c : helper) x =
    if c#isVar x
    then to_var c x (reify r1 r2 r3 r4)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) (r4 c) x)
end

module Fmap5 (T : T5) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 (c : helper) x =
    if c#isVar x
    then to_var c x (reify r1 r2 r3 r4 r5)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) (r4 c) (r5 c) x)
end

module Fmap6 (T : T6) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 r6 (c : helper) x =
    if c#isVar x then
    to_var c x (reify r1 r2 r3 r4 r5 r6)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) (r4 c) (r5 c) (r6 c) x)
end

let rec reify (c : helper) n =
  if c#isVar n
  then to_var c n reify
  else Value n

exception Not_a_value
exception Occurs_check

let to_logic x = Value x

let from_logic = function
| Value x    -> x
| Var (_, _) -> raise Not_a_value

let (!!) x = inj (lift x)

module M = Map.Make (struct type t = int let compare = (-) end)
module Int = struct type t = int let compare = (-) end

module VarSet = Set.Make(Var)

module Env :
  sig
    type t

    val empty     : unit -> t
    val fresh     : (*?name:string -> *)scope:Var.scope -> t -> 'a
    val var       : t -> 'a -> int option
    val is_var    : t -> 'a -> bool
    val free_vars : t -> 'a -> VarSet.t
    val merge     : t -> t -> t
  end =
  struct
    type t = {anchor : Var.env; mutable next : int}

    let last_anchor = ref 11

    let empty () =
      incr last_anchor;
      {anchor = !last_anchor; next = 10}

    let fresh (*?name *) ~scope e =
      let v = !!!(Var.make ~env:e.anchor ~scope e.next) in
      e.next <- 1 + e.next;
      !!!v

    let var_tag, var_size =
      let index = 0 in (* dummy index *)
      let env   = 0 in (* dummy env token *)
      let scope = 0 in (* dummy scope *)
      let v = Var.make ~env ~scope index in
      Obj.tag !!!v, Obj.size !!!v

    let var env x =
      let t = !!! x in
      if Obj.tag  t = var_tag  &&
         Obj.size t = var_size &&
         (let token = (!!!x : Var.t).Var.anchor in (Obj.is_block !!!token) && token == !!!Var.global_anchor)
      then
        let q = (!!!x : Var.t).Var.env in
        if (Obj.is_int !!!q) && q == env.anchor
        then Some (!!!x : Var.t).index
        else failwith "OCanren fatal (Env.var): wrong environment"
      else None

    let is_var env v = None <> var env v

    let free_vars env x =
      let rec helper fv t =
        if is_var env t
        then VarSet.add (!!!t : Var.t) fv
        else
          match wrap t with
          | Unboxed vx -> fv
          | Boxed (tx, sx, fx) ->
            let rec inner fv i =
              if i < sx
              then inner (helper fv !!!(fx i)) (i+1)
              else fv
            in
            inner fv 0
          | Invalid n -> failwith (sprintf "OCanren fatal (Env.free_vars): invalid value (%d)" n)
      in
      helper VarSet.empty (Obj.repr x)

    let merge {anchor=anchor1; next=next1} {anchor=anchor2; next=next2} =
      assert (anchor1 == anchor2);
      {anchor=anchor1; next = max next1 next2}
  end

module Subst :
  sig
    type t
    type content = {var : Var.t; term : Obj.t }

    val empty : t

    val of_list : content list -> t

    val split : t -> content list

    val walk  : Env.t -> t -> 'a -> 'a

    val deepwalk : ?walk_ctrs:(Var.t -> 'a list) -> Env.t -> t -> 'a -> 'a

    val is_bound : Var.t -> t -> bool

    val free_vars : Env.t -> t -> 'a -> VarSet.t

    (* [unify env x y scope subst] returns None if two terms are not unifiable.
     *   Otherwise it returns a pair of prefix and new substituion.
     *   Prefix is a list of pairs (var, term) that were added to the original substituion.
     *)
    val unify   : Env.t -> 'a -> 'a -> scope:Var.scope -> t -> (content list * t) option

    (* [merge env s1 s2] merges two substituions *)
    val merge : Env.t -> t -> t -> t option

    (* [is_subsumed env s1 s2] checks that s1 is subsumed by s2 (i.e. s2 is more general than s1) *)
    val is_subsumed : Env.t -> t -> t -> bool
  end =
  struct
    type content = {var : Var.t; term : Obj.t }
    type t       = content M.t

    let empty = M.empty

    let of_list =
      ListLabels.fold_left ~init:empty ~f:(fun subst cnt ->
        M.add cnt.var.index cnt subst
      )

    let split s = M.fold (fun _ x xs -> x::xs) s []

    let rec walk env subst t =
      if Env.is_var env t
      then
        let v = (!!!t : Var.t) in
        match v.subst with
        | Some term -> walk env subst !!!term
        | None ->
            try walk env subst (Obj.obj (M.find v.index subst).term)
            with Not_found -> t
      else t

    let deepwalk ?(walk_ctrs = fun _ -> []) env subst x =
      let rec helper forbidden t =
        let var = walk env subst t in
        match Env.var env var with
        | None ->
          begin match wrap (Obj.repr var) with
          | Unboxed _ -> Obj.repr var
          | Boxed (tag, sx, fx) ->
            let copy = Obj.dup (Obj.repr var) in
            let sf =
              if tag = Obj.double_array_tag
              then !!!Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to sx-1 do
              sf copy i @@ helper forbidden (!!!(fx i))
            done;
            copy
          | Invalid n -> failwith (sprintf "OCanren fatal (Subst.deepwalk): invalid value (%d)" n)
          end
        | Some n when List.mem n forbidden -> Obj.repr var
        | Some n ->
            let cs =
              walk_ctrs !!!var |>
              List.filter (fun t ->
                match Env.var env t with
                | Some i  -> not (List.mem i forbidden)
                | None    -> true
              ) |>
              List.map (fun t -> !!!(helper (n::forbidden) t))
            in
            Obj.repr {!!!var with Var.constraints = cs}
      in
      !!!(helper [] x)

    let free_vars env subst x =
      Env.free_vars env @@ deepwalk env subst x

    let is_bound var subst = M.mem var.Var.index subst

    let rec occurs env xi term subst =
      let y = walk env subst term in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
         match wy with
         | Invalid n when n = Obj.closure_tag -> false
         | Unboxed _ -> false
         | Invalid n -> failwith (sprintf "OCanren fatal (Subst.occurs): invalid value (%d)" n)
         | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
              else occurs env xi (!!!(f i)) subst || inner (i+1)
            in
            inner 0

    let unify env x y ~scope main_subst =

      (* The idea is to do the unification and collect the unification prefix during the process.
         It is safe to modify variables on the go. There are two cases:
         * if we do unification just after a conde, then the scope is already incremented and nothing goes into
           the fresh variables.
         * if we do unification after a fresh, then in case of failure it doesn't matter if
           the variable is be distructively substituted: we will not look on it in future.
      *)

      let extend xi x term (prefix, sub1) =
        if occurs env xi term sub1 then raise Occurs_check
        else
          let cnt = {var = x; term = Obj.repr term} in
          assert (Env.var env x <> Env.var env term);
          let sub2 =
            if scope = x.Var.scope
            then begin
              x.subst <- Some (Obj.repr term);
              sub1
            end
            else M.add x.Var.index cnt sub1
          in
          Some (cnt::prefix, sub2)
      in
      let rec helper x y : (content list * t) option -> _ = function
        | None -> None
        | Some ((delta, subs) as pair) as acc ->
            let x = walk env subs x in
            let y = walk env subs y in
            match Env.var env x, Env.var env y with
            | (Some xi, Some yi) when xi = yi -> acc
            | (Some xi, Some _) -> extend xi x y pair
            | Some xi, _        -> extend xi x y pair
            | _      , Some yi  -> extend yi y x pair
            | _ ->
                let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
                (match wx, wy with
                 | Unboxed vx, Unboxed vy -> if vx = vy then acc else None
                 | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy
                    then
                      let rec inner i = function
                        | None -> None
                        | (Some delta) as rez ->
                          if i < sx
                          then inner (i+1) (helper (!!!(fx i)) (!!!(fy i)) rez)
                          else rez
                      in
                      inner 0 acc
                    else None
                 | Invalid n, _
                 | _, Invalid n -> failwith (sprintf "OCanren fatal (Subst.unify): invalid value (%d)" n)
                 | _ -> None
                )
      in
      try helper !!!x !!!y (Some ([], main_subst))
      with Occurs_check -> None

      let merge env subst1 subst2 = M.fold (fun _ {var; term} -> function
        | Some s  -> begin
          match unify env !!!var term ~scope:Var.non_local_scope s with
          | Some (_, s') -> Some s'
          | None         -> None
          end
        | None    -> None
      ) subst1 (Some subst2)

      let is_subsumed env subst =
        M.for_all (fun _ {var; term} ->
          match unify env !!!var term ~scope:Var.non_local_scope subst with
          | None          -> false
          | Some ([], _)  -> true
          | Some (_ , _)  -> false
        )

  end

exception Disequality_violated
exception Disequality_fulfilled

module Disequality :
  sig
    type t

    val empty  : t

    (* [of_disj env subst] build a disequality constraint store from a list of bindings
     *   Ex. [(x, 5); (y, 6)] --> (x =/= 5) \/ (y =/= 6)
     *)
    val of_disj : Env.t -> Subst.content list -> t

    (* [of_conj env subst] build a disequality constraint store from a list of bindings
     *   Ex. [(x, 5); (y, 6)] --> (x =/= 5) /\ (y =/= 6)
     *)
    val of_conj : Env.t -> Subst.content list -> t

    (* [check ~prefix env subst diseq] - checks that disequality is not violated in refined substitution.
     *   [prefix] is a substitution prefix, i.e. new bindings obtained during unification.
     *   This function may rebuild internal representation of constraints and thus it returns new object.
     *   Raises [Disequality_violated].
     *)
    val check  : prefix:Subst.content list -> Env.t -> Subst.t -> t -> t

    (* [extend ~prefix env diseq] - extends disequality with new bindings.
     *   New bindings are interpreted as formula in DNF.
     *   Ex. [(x, 5); (y, 6)] --> (x =/= 5) \/ (y =/= 6)
     *)
    val extend : prefix:Subst.content list -> Env.t -> t -> t

    (* [project env subst diseq x] projects [diseq] to free-variables, mentioned in [fv],
     *   i.e. it extracts only those constraints that mention at least one variable from FV(x)
     *)
    val project : Env.t -> Subst.t -> t -> 'a -> t

    (* [normalize env subst diseq x] projects [diseq] to free-variables, mentioned in [fv], and then normalizes them.
     *   Normalization produces a list of disequality constraints stores of the form [(x =/= 5) /\ (y =/= 6)],
      *  i.e. it is a conjunction of individual disequalities.
     *)
    val normalize : Env.t -> Subst.t -> t -> 'a -> t list

    (* [merge env diseq1 diseq2] - merges two disequality stores *)
    val merge : Env.t -> t -> t -> t


    val reify : Env.t -> Subst.t -> t -> Var.t -> 'a list
  end =
  struct
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
     * We choose another `sample` {y =/= u} and add binding to the map for variable {y}.
     * There is no need to check previous samples in the future (because its assumption is already broken in current substitution)
    *)

    module Disjunction :
      sig
        (* Disjunction.t is a set of single disequalities joint by disjunction *)
        type t

        (* [of_list diseqs] build single disjunction from list of disequalities *)
        val of_list : Subst.content list -> t

        (* [check env subst disj] - checks that disjunction of disequalities is
         *   not violated in (current) substitution.
         *   This function is designed to incrementally refine disequalities
         *   with a series of more and more specialized substitutions.
         *   If arbitary substitutions are passed the result may be invalid.
             *)
        val check : Env.t -> Subst.t -> t -> t

        (* [refine env subst disj] - returns `disequality` prefix along with substitution specialized with that prefix.
         *   It is used in two cases:
         *   1) When we want to `negate` a state of search we try to unify current substitution with disequalities.
         *        If unification succeeds we obtain a new specialized state - a counterexample.
         *        Otherwise the substitution with disequalities forms a tautology and we can skip them.
         *   2) When we want to reify an answer we again try to unify current substitution with disequalities.
         *        Then we look into `disequality` prefix for bindings that should not be met.
         *)
        val refine : Env.t -> Subst.t -> t -> (Subst.content list * Subst.t) option

        (* returns an index of variable involved in some disequality inside disjunction *)
        val index : t -> int

        val reify : Var.t -> t -> 'a
      end =
      struct
        type t = { sample : Subst.content; unchecked : Subst.content list }

        let choose_sample unchecked =
          assert (unchecked <> []);
          { sample = List.hd unchecked; unchecked = List.tl unchecked; }

        (* TODO check that list is valid substitution
           (i.e. no different disequalities for same variable. Example: (x =/= 1) || (x =/= 2) is invalid) *)
        let of_list = choose_sample

        type status =
          | Fulfiled
          | Violated
          | Refined of Subst.content list * Subst.t

        let refine' env subst =
        let open Subst in fun { var; term } ->
          match unify env !!!var term Var.non_local_scope subst with
          | None                  -> Fulfiled
          | Some ([], _)          -> Violated
          | Some (prefix, subst)  -> Refined (prefix, subst)

        let rec check env subst {sample; unchecked} =
          match refine' env subst sample with
          | Fulfiled            -> raise Disequality_fulfilled
          | Refined (delta, _)  -> choose_sample (delta @ unchecked)
          | Violated            ->
            match unchecked with
            | [] -> raise Disequality_violated
            | ds -> check env subst @@ choose_sample ds

        let refine env subst {sample; unchecked} =
          let result = ListLabels.fold_left (sample::unchecked) ~init:(Some ([], subst))
                ~f:(fun acc diseq -> match acc with
                    | None -> None
                    | Some (delta, subst) ->
                      match refine' env subst diseq with
                      | Fulfiled                  -> None
                      | Violated                  -> acc
                      | Refined (delta', subst')  -> Some (delta'@delta, subst')
                )
            in
            (* We should not get empty substituion delta here,
             * because it would mean that disequality is violated.
             * But we had to detect violations during search via `check`. *)
            assert (match result with Some ([], _) -> false | _ -> true);
            result

        let reify var {sample; unchecked} =
          if unchecked <> []
          then invalid_arg "OCanren fatal (Disequality.reify): attempting to reify unnormalized disequalities"
          else
            assert (var.Var.index = sample.Subst.var.Var.index);
            !!!(sample.Subst.term)

        let index {sample} = sample.Subst.var.index
      end

    module Index :
      sig
        type t

        val empty     : t
        val add       : int -> Disjunction.t -> t -> t
        val get       : int -> t -> Disjunction.t list
        val replace   : int -> Disjunction.t list -> t -> t
        val fold      : ('a -> Disjunction.t -> 'a) -> 'a -> t -> 'a
        val merge     : t -> t -> t
      end =
      struct
        type t = Disjunction.t list M.t

        let empty           = M.empty
        let get k m         = try M.find k m with Not_found -> []
        let add k v m       = M.add k (v::get k m) m
        let replace k vs m  = M.add k vs (M.remove k m)
        let fold f acc m    = M.fold (fun _ disjs acc -> ListLabels.fold_left ~init:acc ~f disjs) m acc
        let merge           = M.union (fun _ d1 d2-> Some (d1 @ d2))
      end

    type t = Index.t

    let empty = Index.empty

    let extend ~prefix env cstore =
      if prefix=[]
      then cstore
      else
        let disj = Disjunction.of_list prefix in
        Index.add (Disjunction.index disj) disj cstore

    let of_disj env disjs =
      extend ~prefix:disjs env empty

    let of_conj env conjs =
      ListLabels.fold_left conjs ~init:empty
        ~f:(fun acc pair -> extend ~prefix:[pair] env acc)

    let check ~prefix env subst cstore =
      let revisit_conjuncts var_idx conj =
        ListLabels.fold_left conj
            ~init:([], [])
            ~f:(fun (stayed, rebound) disj ->
              try
                let disj = Disjunction.check env subst disj in
                if var_idx = (Disjunction.index disj)
                then (disj::stayed, rebound)
                else (stayed, disj::rebound)
              with Disequality_fulfilled -> (stayed, rebound)
            )
      in
      ListLabels.fold_left prefix ~init:cstore
        ~f:(fun cstore cnt ->
          let var_idx = cnt.Subst.var.index in
          let conj = Index.get var_idx cstore in
          let stayed1, rebound1 = revisit_conjuncts var_idx conj in
          let cstore, rebound2 = match Env.var env cnt.Subst.term with
            | Some n ->
              let stayed2, rebound2 = revisit_conjuncts n @@ Index.get n cstore in
              Index.replace n stayed2 cstore, rebound2
            | None   -> cstore, []
          in
          let cstore = Index.replace var_idx stayed1 cstore in
          let extend rebound cstore =
            ListLabels.fold_left rebound ~init:cstore
            ~f:(fun cstore disj ->
              Index.add (Disjunction.index disj) disj cstore
            )
          in
          let cstore = extend rebound1 cstore in
          let cstore = extend rebound2 cstore in
          cstore
        )

    let reify env subst t var =
      let conjs = Index.get var.Var.index t in
      List.map (Disjunction.reify var) conjs

    let merge env = Index.merge

    (* [refine env subst diseqs] - takes a [subst] and refines it w.r.t [diseqs].
     *   For each disequality substitution in constraint's store this function tries to unify
     *   disequality substitution with [subst].
     *   If unification succeeds the unification prefix along with refined substitution is added to the result list.
     *   Function returns a list of tuples [(delta, delta-subst, subst)] where
     *     1) delta - unification prefix
     *     2) delta-subst - unification prefix as substitution
     *     3) subst - refined substitution
     *   This function is used in two cases:
     *     1) For `negation` of a state of search.
     *     2) For answer reification.
     *   See Disjunction.refine for details.
     *)
    let refine env subst t =
      Index.fold (fun acc disj ->
        match Disjunction.refine env subst disj with
        | None -> acc
        | Some (delta, s) ->
          let delta_subst = Subst.of_list delta in
          let is_subsumed = ListLabels.exists acc
            ~f:(fun _,delta_subst',_ -> Subst.is_subsumed env delta_subst delta_subst')
          in
          if is_subsumed
          then acc
          else
            let acc = ListLabels.filter acc
              ~f:(fun _,delta_subst',_ -> not @@ Subst.is_subsumed env delta_subst' delta_subst)
            in
            (delta, delta_subst, s)::acc
      ) [] t

    let project' env subst t x =
      let cs = refine env subst t |> List.map (fun delta,_,_ -> delta) in
      (* fixpoint-like computation of disequalities relevant to variables in [fv] *)
      let rec helper fv =
        let open Subst in
        let is_relevant fv {var; term} =
          (VarSet.mem var fv) ||
          (match Env.var env term with Some _ -> VarSet.mem (!!!term) fv | None -> false)
        in
        (* filter irrelevant binding in each disjunction *)
        let cs' = List.map (List.filter (is_relevant fv)) cs in
        (* obtain a set of free variables from terms mentioned in disequalities *)
        let fv' = ListLabels.fold_left cs' ~init:fv ~f:(fun acc disj ->
          ListLabels.fold_left disj ~init:acc ~f:(fun acc {var; term} ->
            VarSet.union acc @@ Subst.free_vars env subst term
          )
        ) in
        if VarSet.equal fv fv'
        then cs'
        else helper fv'
      in
      helper @@ Subst.free_vars env subst x

    let project env subst t x =
      project' env subst t x |>
      (* build constraints back from list representation *)
      ListLabels.fold_left ~init:empty ~f:(fun acc disj -> extend ~prefix:disj env acc)

    let normalize env subst t x =
      project' env subst t x |>
      ListLabels.fold_left ~init:[[]]
        ~f:(fun acc disj ->
          ListLabels.map acc ~f:(fun conj ->
            let disj = List.filter (fun x -> not @@ List.exists ((=) x) conj) disj in
            List.map (fun x -> x::conj) disj
          ) |> List.concat
        ) |>
      List.map (of_conj env)
end

module State =
  struct
    type t =
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Disequality.t
      ; scope : Var.scope
      }

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

    let new_var {env; scope} =
      let x = Env.fresh ~scope env in
      let i = (!!!x : Var.t).Var.index in
      (x,i)

    let incr_scope st = {st with scope = Var.new_scope ()}

    let merge
      {env=env1; subst=subst1; ctrs=ctrs1; scope=scope1}
      {env=env2; subst=subst2; ctrs=ctrs2; scope=scope2} =
      let env = Env.merge env1 env2 in
      match Subst.merge env subst1 subst2 with
      | None       -> None
      | Some subst -> Some
        { env; subst
        ; ctrs  = Disequality.merge env ctrs1 ctrs2
        ; scope = Var.new_scope ()
        }

    let project ({env; subst; ctrs} as st) x =
      {st with ctrs = Disequality.project env subst ctrs x}

    let normalize ({env; subst; ctrs} as st) x =
      match Disequality.normalize env subst ctrs x with
      | []    -> [{st with ctrs=Disequality.empty}]
      | ctrss ->
        List.map (fun ctrs -> {env; subst; ctrs; scope = Var.new_scope ()}) ctrss

    let reify {env; subst; ctrs} x =
      Subst.deepwalk ~walk_ctrs:(Disequality.reify env subst ctrs) env subst x

  end

type 'a goal' = State.t -> 'a
type goal = State.t Stream.internal goal'

let success st = Stream.Internal.single st
let failure _  = Stream.Internal.nil

let call_fresh f =
  let open State in fun ({env; scope} as st) ->
  let x = Env.fresh ~scope env in
  f x st

let (===) (x: _ injected) y =
  let open State in fun ({env; subst; ctrs; scope} as st) ->
  LOG[perf] (Log.unify#enter);
  let result =
    match Subst.unify env x y scope subst with
    | None -> Stream.Internal.nil
    | Some (prefix, s) ->
      try
        let ctrs' = Disequality.check ~prefix env s ctrs in
        Stream.Internal.single {st with subst=s; ctrs=ctrs'}
      with Disequality_violated -> Stream.Internal.nil
  in
  LOG[perf] (Log.unify#leave);
  result

let (=/=) x y =
  let open State in fun ({env; subst; ctrs; scope} as st) ->
  match Subst.unify env x y Var.non_local_scope subst with
  | None         -> Stream.Internal.single st
  | Some ([], _) -> Stream.Internal.nil
  | Some (prefix, _) ->
      let ctrs' = Disequality.extend ~prefix env ctrs in
      Stream.Internal.single {st with ctrs=ctrs'}

let delay g st = Stream.Internal.from_fun (fun () -> g () st)

let conj f g st = Stream.Internal.bind (f st) g
let (&&&) = conj
let (?&) gs = List.fold_right (&&&) gs success

let disj_base f g st = Stream.Internal.mplus (f st) (Stream.Internal.from_fun (fun () -> g st))

let disj f g st = let st = State.incr_scope st in disj_base f g |> (fun g -> Stream.Internal.inc (fun () -> g st))

let (|||) = disj

let (?|) gs st =
  let st = State.incr_scope st in
  let rec inner = function
  | [g]   -> g
  | g::gs -> disj_base g (inner gs)
  | [] -> failwith "Wrong argument of (?!)"
  in
  inner gs |> (fun g -> Stream.Internal.inc (fun () -> g st))

let conde = (?|)

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

exception FreeVarFound

let has_free_vars is_var x =
  let rec walk x =
    if is_var x then raise FreeVarFound
    else
      match wrap (Obj.repr x) with
      | Boxed (_tag, size, f) ->
        for i = 0 to size - 1 do
          walk (!!!(f i))
        done
      | _ -> ()
  in
  try walk x; false
  with FreeVarFound -> true

let helper_of_state st =
  !!!(object method isVar x = Env.is_var (State.env st) (Obj.repr x) end)

class type ['a,'b] reified = object
  method is_open : bool
  method prj     : 'a
  method reify   : (helper -> ('a, 'b) injected -> 'b) -> inj:('a -> 'b) -> 'b
end

let make_rr : ('a, 'b) injected -> State.t -> ('a, 'b) reified =
  let open State in fun x ({env; subst; ctrs;} as st) ->
  let ans = !!!(State.reify st (Obj.repr x)) in
  let is_open = has_free_vars (Env.is_var env) (Obj.repr ans) in
  let c: helper = helper_of_state st in
  object (self)
    method is_open            = is_open
    method prj                = if self#is_open then raise Not_a_value else !!!ans
    method reify reifier ~inj = if self#is_open then reifier c ans else inj ans
  end

let prj x = let rr = make_rr x @@ State.empty () in rr#prj

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module R :
  sig
    val apply_reifier : State.t Stream.t -> ('a, 'b) injected -> ('a, 'b) reified Stream.t
  end =
  struct
    let apply_reifier stream x = Stream.map (make_rr x) stream
  end

module ApplyTuple =
  struct
    let one arg r = R.apply_reifier arg r
    let succ prev = fun arg (r, y) -> (R.apply_reifier arg r, prev arg y)
  end

module Uncurry =
  struct
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

let succ n () =
  let adder, currier, app, ext = n () in
  (LogicAdder.succ adder, Uncurry.succ currier, ApplyTuple.succ app, ExtractDeepest.succ ext)

let one   () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyTuple.one, ExtractDeepest.ext2
let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let pqrst = five

let run n goalish f =
  let adder, currier, app, ext = n () in
  Log.clear ();
  LOG[perf] (Log.run#enter);
  let helper tup =
    let args, stream = ext tup in
    (* we normalize stream before reification *)
    let stream =
      Stream.Internal.bind stream (fun st -> Stream.Internal.of_list @@ State.normalize st args)
    in
    currier f @@ app (Stream.of_mkstream stream) args
  in
  let result = helper (adder goalish @@ State.empty ()) in
  LOG[perf] (
    Log.run#leave;
    printf "Run report:\n%s" @@ Log.report ()
  );
  result

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

  if Stream.Internal.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans

let diseqtrace shower x y = fun st ->
  incr logged_diseq_counter;
  let ans = (x =/= y) st in
  (* printf "%d: (=/=) '%s' and '%s'" !logged_diseq_counter
    (shower (helper_of_state st) x)
    (shower (helper_of_state st) y);
  if Stream.Internal.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans;;

(* ***************************** a la relational StdLib here ***************  *)

let report_counters () = ()
*)
