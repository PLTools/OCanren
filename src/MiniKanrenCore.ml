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

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : ?name:string -> scope:Var.scope -> t -> 'a * t
    val var    : t -> 'a -> int option
    val is_var : t -> 'a -> bool
  end =
  struct
    type t = {anchor : Var.env; mutable next : int}

    let last_anchor = ref 11

    let empty () =
      incr last_anchor;
      {anchor = !last_anchor; next = 10}

    let fresh ?name ~scope e =
      let v = !!!(Var.make ~env:e.anchor ~scope e.next) in
      e.next <- 1 + e.next;
      (!!!v, e)

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
        if (Obj.is_int !!!q) && q == !!!env.anchor
        then Some (!!!x : Var.t).index
        else failwith "OCanren fatal (Env.var): wrong environment"
      else None

    let is_var env v = None <> var env v
  end

module Subst :
  sig
    type t
    type content = {var : Var.t; term : Obj.t }

    val empty : t

    val split : t -> Var.t list * Obj.t list
    val walk  : Env.t -> 'a -> t -> 'a

    val merge_prefix_unsafe : scope : Var.scope -> content list  -> t -> t
    val merge_prefix        : Env.t -> scope:Var.scope -> content list -> t -> (t * bool) option

    val unify   : Env.t -> 'a -> 'a -> scope:Var.scope -> t -> (content list * t) option
  end =
  struct
    type content = {var : Var.t; term : Obj.t }
    type t       = content M.t

    let empty = M.empty

    let split s = M.fold (fun _ {var; term} (xs, ts) -> (var::xs, term::ts)) s ([], [])

    let lookup ui u map =
      match u.Var.subst with
      | Some term -> {var = u; term}
      | None      -> M.find !!!ui map

    let rec walk env t subst =
      let rec helper x =
        if Env.is_var env x
        then
          let v = (!!!x : Var.t) in
          match v.subst with
          | Some term -> walk env !!!term subst
          | None ->
              try walk env (Obj.obj (lookup v.index !!!v subst).term) subst
              with Not_found -> x
        else t
      in
      helper t

    let rec occurs env xi term subst =
      let y = walk env term subst in
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

    let merge_prefix_unsafe ~scope prefix subst =
      ListLabels.fold_left prefix ~init:subst ~f:(fun acc cnt ->
        if scope = cnt.var.Var.scope
        then (
          cnt.var.subst <- Some cnt.term;
          acc
        )
        else M.add cnt.var.index cnt acc
      )

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
          let sub2 = merge_prefix_unsafe ~scope [cnt] sub1 in
          Some (cnt :: prefix, sub2)
      in
      let rec helper x y : (content list * t) option -> _ = function
        | None -> None
        | Some ((delta, subs) as pair) as acc ->
            let x = walk env x subs in
            let y = walk env y subs in
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

    let merge_prefix env ~scope prefix subst =
      let rec helper is_enlarged acc = function
      | [] -> Some (acc, is_enlarged)
      | h::tl ->
          match unify env ~scope !!!h.var !!!h.term acc with
          | None       -> None
          | Some (p,s) -> helper (is_enlarged || p <> []) s tl
      in
      helper false subst prefix

  end

let rec reify' env subst do_diseq x =
  let rec walk' forbidden term =
    let var = Subst.walk env term subst in
    match Env.var env var with
    | None ->
        (match wrap (Obj.repr var) with
         | Unboxed _ -> Obj.repr var
         | Boxed (t, s, f) ->
            let copy = Obj.dup (Obj.repr var) in
            let sf =
              if t = Obj.double_array_tag
              then !!!Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to s-1 do
              sf copy i @@ walk' forbidden (!!!(f i))
            done;
            copy
         | Invalid n -> failwith (sprintf "OCanren fatal (reify'): invalid value (%d)" n)
        )
    | Some n when List.mem n forbidden -> var
    | Some n ->
        let cs : _ list = do_diseq !!!var in
        let cs = List.filter (fun x -> match Env.var env x with Some n -> not (List.mem n forbidden) | None -> true) cs in
        let cs = List.map (walk' ((!!!var : Var.t).index :: forbidden)) cs in
        Obj.repr {!!!var with Var.constraints = cs}
  in
  walk' [] x

exception Disequality_violated

module Constraints :
  sig
    type t

    val empty  : t
    val check  : prefix:Subst.content list -> Env.t -> Subst.t -> t -> t
    val extend : prefix:Subst.content list -> Env.t -> t -> t
    val reify  : Env.t -> Subst.t -> t -> Var.t -> 'a list
  end =
  struct
    type single = Subst.content list

    module M =
      struct
        include M

        let empty            = empty
        let find_exn         = find
        let find     key map = try find_exn key map with Not_found -> []
        let add      k v m   = try add k (v::find_exn k m) m with Not_found -> add k [v] m
        let replace  k v     = if v = [] then remove k else M.add k v
      end

    type t = single list M.t

    let empty = M.empty

    let extend ~prefix env cs =
      assert (prefix <> []);
      Subst.(
        let h   = List.hd prefix in
        let ans = M.add h.var.index prefix cs in
        match Env.var env h.term with
        | None -> ans
        | Some n ->
            let swapped = {term = Obj.repr h.var; var = (!!!(h.term) : Var.t)} in
            M.add n (swapped::(List.tl prefix)) ans
      )

    let split_and_unify ~prefix env subst =
      Subst.(
        let vs,ts = List.split @@ List.map (fun {var; term} -> (var, term)) prefix in
        unify env !!!vs !!!ts Var.non_local_scope subst
      )

    let interacts_with ~prefix c =
      let first_var = (List.hd c).Subst.var in
      try Some (List.find (fun cnt -> cnt.Subst.var.index = first_var.index) prefix)
      with Not_found ->
        match first_var.subst with
        | Some term -> Some {Subst.var=first_var; Subst.term=term}
        | None      -> None

    type revisiting_result = Obsolete | Reworked of int * single | Violated

    let check ~prefix env subst c_store =
      let revisit_constraint c =
        let rec helper = function
        | []    -> Violated
        | h::tl ->
            match Subst.(unify env !!!(h.Subst.var) h.Subst.term) Var.non_local_scope subst with
            | None                             -> Obsolete
            | Some ([], _)                     -> helper tl
            | Some ((ph::_) as new_prefix, _)  -> Reworked (ph.Subst.var.index, new_prefix@tl)
        in
        helper c
      in
      let rec loop2 map = function
      | [] -> map
      | h :: tl ->
          let important = M.find h.Subst.var.index map in
          let (acc_cur, acc_other) =
            ListLabels.fold_left important ~init:([], [])
                ~f:(fun (acc_cur, acc_other) cs ->
                      match revisit_constraint cs with
                      | Violated                                             -> raise Disequality_violated
                      | Reworked (idx, new_one) when idx = h.Subst.var.index -> (new_one::acc_cur, acc_other)
                      | Reworked (idx, new_one)                              -> (acc_cur, (idx, new_one)::acc_other)
                      | Obsolete                                             -> (acc_cur, acc_other)
                   )
          in
          let map = M.replace h.Subst.var.index acc_cur map in
          let map = ListLabels.fold_left ~init:map acc_other ~f:(fun acc (i, cs) -> M.add i cs acc) in
          loop2 map tl
      in
      loop2 c_store prefix

    let is_subsumed env d d2 =
      let s = Subst.merge_prefix_unsafe ~scope:Var.non_local_scope d Subst.empty in
      let rec helper = function
      | [] -> false
      | h::tl ->
          match Subst.merge_prefix env ~scope:Var.non_local_scope h s with
          | None            -> helper tl
          | Some (_, false) -> true
          | Some (__, _)    -> helper tl
    in
    helper d2

    let rem_subsumed env cs =
      let rec helper d acc =
        match d with
        | []                                                       -> acc
        | h::tl when is_subsumed env h tl || is_subsumed env h acc -> helper tl acc
        | h:: tl                                                   -> helper tl (h::acc)
    in
    helper cs []

    exception ReallyNotEqual

    let simplify_single ~env ~subst asked_var maybe_swap single =
      (* We need this to simplify answer in that case:
       *   subst: [ q -> (a,b); ]
       *   constr: [ (a=/=5) || (b=/=6) ]
       *   extend subst with (a === 3)
       *   ask to reify: q
       *   expected answer: q = (3, b)
       *   without simplification: q = (3, b {{ =/= 6}})
       *)
      let rec helper acc = function
      | []       -> acc
      | cont::tl ->
          match Subst.(unify env !!!(cont.var) !!!cont.term Var.non_local_scope subst) with
          | None         -> raise ReallyNotEqual
          | Some ([], _) ->
              if cont.Subst.var != asked_var && cont.Subst.term != !!!asked_var
              then []
              else helper acc tl
          | Some (_, _) -> helper ((maybe_swap cont) :: acc) tl
      in
      try helper [] single with ReallyNotEqual -> []

    let rem_subsumed_opt ~env ~subst asked_var maybe_swap cs_map  =
      M.fold (fun k cs_list acc ->
                ListLabels.fold_left ~init:acc cs_list
                                     ~f:(fun acc single ->
                                           let single = simplify_single ~env ~subst asked_var maybe_swap single in
                                           try
                                             ignore (List.find (fun x -> (x.Subst.var == asked_var) || (x.Subst.term == !!!asked_var)) single);
                                             if is_subsumed env single acc then acc else single::acc
                                           with Not_found -> acc
                                        )
             )
             cs_map
             []

    let rem_duplicates xs =
      let rec loop acc = function
      | []                         -> acc
      | h::tl when List.memq h acc -> loop acc tl
      | h::tl                      -> loop (h::acc) tl
      in
      loop [] xs

    let reify env subst cs term =
      let maybe_swap cnt =
        Subst.(if cnt.term == !!!term then {var = !!!term; term = Obj.repr cnt.var} else cnt)
      in
      let ans =
        ListLabels.map ~f:(fun prefix ->
                             let cs_sub = Subst.merge_prefix_unsafe ~scope:Var.non_local_scope prefix Subst.empty in
                             let dest = Subst.walk env !!!term cs_sub in
                             assert (term <> dest);
                             dest
                          )
                          (rem_subsumed_opt ~env ~subst term maybe_swap cs)
      in
      rem_duplicates ans
end

module State =
  struct
    type t =
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Constraints.t
      ; scope : Var.scope
      }

    let empty () =
      { env   = Env.empty ()
      ; subst = Subst.empty
      ; ctrs  = Constraints.empty
      ; scope = Var.new_scope ()
      }

    let env   {env;} = env
    let subst {subst;} = subst
    let constraints {ctrs;} = ctrs

    let new_var {env; scope} =
      let (x,_) = Env.fresh ~scope env in
      let i = (!!!x : Var.t).index in
      (x,i)

    let incr_scope {scope} as st = {st with scope = Var.new_scope ()}
  end

type 'a goal' = State.t -> 'a
type goal = State.t Stream.internal goal'

let success st = Stream.Internal.single st
let failure _  = Stream.Internal.nil

let call_fresh f =
  let open State in fun {env; scope} as st ->
    let x, env' = Env.fresh ~scope env in
    f x {st with env=env'}

let (===) (x: _ injected) y =
  let open State in fun {env; subst; ctrs; scope} as st ->
  LOG[perf] (Log.unify#enter);
  let result =
    match Subst.unify env x y scope subst with
    | None -> Stream.Internal.nil
    | Some (prefix, s) ->
      try
        let ctrs' = Constraints.check ~prefix env s ctrs in
        Stream.Internal.single {st with subst=s; ctrs=ctrs'}
      with Disequality_violated -> Stream.Internal.nil
  in
  LOG[perf] (Log.unify#leave);
  result

let (=/=) x y =
  let open State in fun {env; subst; ctrs; scope} as st ->
  match Subst.unify env x y Var.non_local_scope subst with
  | None         -> Stream.Internal.single st
  | Some ([], _) -> Stream.Internal.nil
  | Some (prefix, _) ->
      let ctrs' = Constraints.extend ~prefix env ctrs in
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
  in
  inner gs |> (fun g -> Stream.Internal.inc (fun () -> g st))

let conde = (?|)

module Fresh =
  struct
    let succ prev f = call_fresh (fun x -> prev (f x))

    let zero  f = f
    let one   f = succ zero f
    let two   f = succ one f
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

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

let helper_of_state st =
  !!!(object method isVar x = Env.is_var (State.env st) (Obj.repr x) end)

class type ['a,'b] reified = object
  method is_open : bool
  method prj     : 'a
  method reify   : (helper -> ('a, 'b) injected -> 'b) -> inj:('a -> 'b) -> 'b
end

let make_rr : ('a, 'b) injected -> State.t -> ('a, 'b) reified =
  let open State in fun x ({env; subst; ctrs;} as st) ->
  let ans = !!!(reify' env subst (Constraints.reify env subst ctrs) (Obj.repr x)) in
  let is_open = has_free_vars (Env.is_var env) (Obj.repr ans) in
  let c: helper = helper_of_state st in
  object (self)
    method is_open            = is_open
    method prj                = if self#is_open then raise Not_a_value else !!!ans
    method reify reifier ~inj = if self#is_open then reifier c ans else inj ans
  end

let prj x = let rr = make_rr x @@ State.empty () in rr#prj

module R :
  sig
    type ('a, 'b) reifier
    val reifier       :  ('a, 'b) injected -> ('a, 'b) reifier
    val apply_reifier : State.t Stream.t -> ('a, 'b) reifier -> ('a, 'b) reified Stream.t
  end =
  struct
    type ('a, 'b) reifier = State.t Stream.t -> ('a, 'b) reified Stream.t

    let reifier       x    =  Stream.map (make_rr x)
    let apply_reifier st r = r st
  end

module ApplyTuple =
  struct
    let one arg r = R.apply_reifier arg r
    let succ prev = fun arg (r, y) -> (R.apply_reifier arg r, prev arg y)
  end

module ApplyLatest =
  struct
    let two = (ApplyTuple.one, ExtractDeepest.ext2)
    let apply (appf, extf) tup =
      let x, base = extf tup in
      appf (Stream.of_mkstream base) x
    let succ (appf, extf) = (ApplyTuple.succ appf, ExtractDeepest.succ extf)
  end

module Uncurry =
  struct
    let succ k f (x,y) = k (f x) y
  end

type ('a, 'b) reifier = ('a, 'b) R.reifier
let reifier = R.reifier

module LogicAdder :
  sig
    val zero : 'a -> 'a
    val succ : ('a -> State.t -> 'd) -> (('e, 'f) injected -> 'a) -> State.t -> ('e, 'f) R.reifier * 'd
  end = struct
    let zero f      = f
    let succ prev f = call_fresh (fun logic st -> (R.reifier logic, prev (f logic) st))
  end


let succ n () =
  let adder, currier, app = n () in
  (LogicAdder.succ adder, Uncurry.succ currier, ApplyLatest.succ app)

let one   () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyLatest.two
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
  let adder, currier, app_num = n () in
  Log.clear ();
  LOG[perf] (Log.run#enter);
  let run f  = f (State.empty ())  in
  let result = run (adder goalish) |> ApplyLatest.apply app_num |> (currier f) in
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

<<<<<<< HEAD
let reify_with_state (env,subs,cs,_) term = reify' env subs (Constraints.reify env subs cs) (Obj.repr term)
=======
let refine_with_state =
  let open State in fun {env; subst; ctrs;} term ->
  refine env subst (Constraints.refine env subst ctrs) (Obj.repr term)
>>>>>>> 2535862... state as a record

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
