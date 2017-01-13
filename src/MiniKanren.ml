(*
 * MiniKanren: miniKanren implementation.
 * Copyright (C) 2015-2016
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin,
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

module Stream =
  struct

    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec is_empty = function
    | Nil    -> true
    | Lazy s -> is_empty @@ Lazy.force s
    | _ -> false

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

    let rec mplus fs gs =
      from_fun (fun () ->
         match fs with
         | Nil           -> gs
         | Cons (hd, tl) -> cons hd (mplus gs tl)
         | Lazy z        -> mplus gs (Lazy.force z)
      )

    let rec bind xs f =
      from_fun (fun () ->
        match xs with
        | Cons (x, xs) -> mplus (f x) (bind xs f)
        | Nil          -> nil
        | Lazy z       -> bind (Lazy.force z) f
     )

    let rec map f = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
    | Lazy s -> Lazy (Lazy.from_fun (fun () -> map f @@ Lazy.force s))

    let rec iter f = function
    | Nil -> ()
    | Cons (x, xs) -> f x; iter f xs
    | Lazy s -> iter f @@ Lazy.force s

  end

let (!!!) = Obj.magic;;
type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let rec wrap (x : Obj.t) =
  Obj.(
    let is_valid_tag =
      List.fold_left
      (fun f t tag -> tag <> t && f tag)
      (fun _ -> true)
      [lazy_tag   ; closure_tag  ; object_tag  ; infix_tag ;
       forward_tag; no_scan_tag  ; abstract_tag; custom_tag;
       custom_tag  ; unaligned_tag; out_of_heap_tag
      ]
    in
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

let generic_show x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner o =
    match wrap o with
    | Invalid n             -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed n when !!!n=0 -> Buffer.add_string b "[]"
    | Unboxed n             -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!!n))
    (* | Boxed (t,l,f) when t=0 && l=1 && (match wrap (f 0) with Unboxed i when !!!i >=10 -> true | _ -> false) ->
       Printf.bprintf b "var%d" (match wrap (f 0) with Unboxed i -> !!!i | _ -> failwith "shit") *)

    | Boxed   (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b

type ('a, 'c) fancy = 'a;;
let bprintf_fancy b f x = f x;;
let show_fancy: ('a -> string) -> ('a,'b) fancy -> string = fun f x -> f x;;

@type 'a logic = | Var of GT.int GT.list * GT.int * 'a logic GT.list
                 | Value of 'a
                   with show,gmap,html,eq,compare,foldl,foldr;;

external coerce_fancy: ('a, 'b) fancy -> 'a = "%identity"
external var_of_fancy: ('a, 'b) fancy -> 'a logic = "%identity"
external cast_fancy:   ('a, 'r) fancy -> 'r = "%identity"

let rec bprintf_logic: Buffer.t -> ('a -> unit) -> 'a logic -> unit = fun b f x ->
  let rec helper = function
  | Value x -> f x
  | Var (_token,i,cs) ->
    bprintf b " _.%d" i;
    List.iter (fun x -> bprintf b "=/= "; helper x) cs
  in
  helper x

let rec show_logic f = function
| Value x -> f x
| Var (_,i,cs) ->
  let c =
    match cs with
    | [] -> ""
    | _  -> sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ (show_logic (fun _ -> assert false) l)) cs)
  in
  sprintf "_.%d%s" i c

let logic = {logic with
 gcata = ();
 plugins =
   object
     method gmap    = logic.plugins#gmap
     method html    = logic.plugins#html
     method eq      = logic.plugins#eq
     method compare = logic.plugins#compare
     method foldl   = logic.plugins#foldl
     method foldr   = logic.plugins#foldr
     method show fa x =
       GT.transform(logic)
          (GT.lift fa)
          (object inherit ['a] @logic[show]
            method c_Var _ s _token i cs = show_logic (fun _ -> assert false) (Var(_token,i,cs))
              (* let c =
              match cs with
              | [] -> ""
              | _  -> Printf.sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
              in
              Printf.sprintf "_.%d%s" i c *)
            method c_Value _ _ x = x.GT.fx ()
           end)
          ()
          x
   end
};;

let lift: 'a -> ('a, 'a) fancy = fun x -> x

let inj: ('a, 'c) fancy -> ('a, 'c) fancy = fun x -> x

let (!!) = inj

let inj_pair : ('a, 'b) fancy -> ('c,'d) fancy -> ('a*'c, 'b*'d) fancy =
  fun x y -> (x,y)

exception Not_a_value

(*
let prj_k k = function Value x -> x | Var (_, i, c) -> k i c
let prj x = prj_k (fun _ -> raise Not_a_value) x

let (!?) = prj
*)
exception Occurs_check

module Int = struct type t = int let compare = Pervasives.compare end
module MultiIntMap : sig
  type key = Int.t
  type 'a t
  val empty: 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_exn: key -> 'a t -> 'a list
  val replace: key -> 'a list -> 'a t -> 'a t
end = struct
  module M = Map.Make(Int)

  type key = Int.t
  type 'a t = 'a list M.t

  let empty : 'a t = M.empty
  let add k v m =
    try let vs = M.find k m in
        let vs = if List.memq v vs then vs else v::vs in
        M.add k vs m
    with Not_found -> M.add k [v] m

  let find_exn : key -> 'a t -> 'a list = M.find
  let replace: key -> 'a list -> 'a t -> 'a t = M.add
end

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : t -> 'a * t
    val var    : t -> 'a -> int option
    val is_var : t -> 'a -> bool
  end =
  struct
    type t = { token : GT.int GT.list;
               mutable next: int;
               mutable reifiers: Obj.t MultiIntMap.t;
               mutable top_vars: int list }

    let empty () = { token=[0]; next=10; reifiers=MultiIntMap.empty; top_vars=[] }

    let fresh e =
      let v = Var (e.token, e.next, []) in
      (!!!v, {e with next=1+e.next})

    let var_tag, var_size =
      let v = Var ([], 0, []) in
      Obj.tag (!!! v), Obj.size (!!! v)

    let var {token=a;_} x =
      let t = !!! x in
      if Obj.tag  t = var_tag  &&
         Obj.size t = var_size &&
         (let q = Obj.field t 0 in
          not (Obj.is_int q) && q == (!!!a)
         )
      then let Var (_, i, _) = !!! x in Some i
      else None
    let is_var env v = None <> var env v
  end

module Subst : sig
    type t

    val empty   : t

    type content = { lvar: Obj.t; new_val: Obj.t }
    val make_content : 'a -> 'b  -> content

    val of_list : (int * content) list -> t
    (* splits substitution into two lists of the same length. 1st contains logic vars,
     * second values to substitute *)
    val split   : t -> Obj.t list * Obj.t list
    val walk    : Env.t -> 'a -> t -> 'a
    val unify   : Env.t -> 'a -> 'a -> t option -> (int * content) list * t option
    val show    : t -> string
  end =
  struct
    module M = Map.Make (Int)

    (* map from var indicies to tuples of (actual vars, value, reifier_func) *)
    type content = { lvar: Obj.t; new_val: Obj.t (*; reifier: unit*) }
    type t = content M.t
    let new_val {new_val=x;_} = Obj.obj x
    let lvar    {lvar=v;_}    = Obj.obj v
    let make_content a b = { lvar=Obj.repr a; new_val=Obj.repr b }

    let show m =
      (* (M.fold (fun i  s -> s ^ sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}" *)
      let b = Buffer.create 40 in
      Buffer.add_string b "subst {";
      M.iter (fun i {lvar;_} -> bprintf b "%d -> %s; " i (generic_show lvar)) m;
      Buffer.add_string b "}";
      Buffer.contents b

    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, cnt) -> M.add i cnt s) empty l

    let split s = M.fold (fun _ {lvar;new_val} (xs, ts) -> (lvar::xs, new_val::ts)) s ([], [])

    let rec walk : Env.t -> 'a -> t -> 'a = fun env var subst ->
      match Env.var env !!!var with
      | None   -> var
      | Some i ->
          try
            let ans = new_val @@ M.find i subst in
            walk env ans subst
          with Not_found -> var

    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
         match wy with
         | Invalid 247
         | Unboxed _ -> false
         | Invalid n -> invalid_arg (sprintf "Invalid value in occurs check (%d)" n)
         | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
              else occurs env xi (!!!(f i)) subst || inner (i+1)
            in
            inner 0

    let unify env x y subst =
      let extend xi x term delta subst =
        if occurs env xi term subst then raise Occurs_check
        else
          let cnt = make_content x term in
          ((xi, cnt)::delta, Some (M.add xi cnt (!!! subst)) )
      in
      let rec unify x y (delta, subst) =
        match subst with
        | None -> delta, None
        | (Some subst) as s ->
            let x, y = walk env x subst, walk env y subst in
            match Env.var env x, Env.var env y with
            | Some xi, Some yi -> if xi = yi then delta, s else extend xi x y delta subst
            | Some xi, _       -> extend xi x y delta subst
            | _      , Some yi -> extend yi y x delta subst
            | _ ->
                let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
                (match wx, wy with
                 (* | Invalid 247, Invalid 247 -> delta,s *)
                 | Unboxed vx, Unboxed vy -> if vx = vy then delta, s else delta, None
                 | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy
                    then
                      let rec inner i (delta, subst) =
                      match subst with
                        | None -> delta, None
                        | Some _ ->
                          if i < sx
                          then inner (i+1) (unify (!!!(fx i)) (!!!(fy i)) (delta, subst))
                          else delta, subst
                      in
                      inner 0 (delta, s)
                    else delta, None
                 | Invalid n, _
                 | _, Invalid n -> invalid_arg (sprintf "Invalid values for unification (%d)" n)
                 | _ -> delta, None
                )
      in
      unify x y ([], subst)

  end

module State =
  struct
    type t = Env.t * Subst.t * Subst.t list
    let empty () = (Env.empty (), Subst.empty, [])
    let env   (env, _, _) = env
    let show  (env, subst, constr) = sprintf "st {%s, %s}" (Subst.show subst) (GT.show(GT.list) Subst.show constr)
  end

type goal = State.t -> State.t Stream.t

let call_fresh f (env, subst, constr) =
  let x, env' = Env.fresh env in
  f x (env', subst, constr)

exception Disequality_violated

let (===) (x: _ fancy) y (env, subst, constr) =
   let () = printf "(===) '%s' and '%s'\n%!" (generic_show x) (generic_show y) in
  (* we should always unify two fancy types *)

  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    begin match subst' with
    | None -> Stream.nil
    | Some s ->
        try
          (* TODO: only apply constraints with the relevant vars *)
          let constr' =
            List.fold_left (fun css' cs ->
              let x,t = Subst.split cs in
              try
                let p, s' = Subst.unify env (!!!x) (!!!t) subst' in
                match s' with
                | None -> css'
                | Some _ ->
                    match p with
                    | [] -> raise Disequality_violated
                    | _  -> (Subst.of_list p)::css'
              with Occurs_check -> css'
            )
            []
            constr
            in
          Stream.cons (env, s, constr') Stream.nil
        with Disequality_violated -> Stream.nil
    end
  with Occurs_check -> Stream.nil

let (=/=) x y ((env, subst, constr) as st) =
  let normalize_store prefix constr =
    let prefix = List.split (List.map Subst.(fun (_, {lvar;new_val}) -> (lvar, new_val)) prefix) in
    (* let subst  = Subst.of_list prefix in
    let prefix = List.split (List.map (fun (_, x, t) -> (x, t)) prefix) in *)
    let subsumes subst (vs, ts) =
      try
        match Subst.unify env !!!vs !!!ts (Some subst) with
        | [], Some _ -> true
        | _ -> false
      with Occurs_check -> false
    in
    let rec traverse = function
    | [] -> [subst]
    | (c::cs) as ccs ->
        if subsumes subst (Subst.split c)
        then ccs
        else if subsumes c prefix
             then traverse cs
             else c :: traverse cs
    in
    traverse constr
  in
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    match subst' with
    | None -> Stream.cons st Stream.nil
    | Some s ->
        (match prefix with
        | [] -> Stream.nil
        | _  -> Stream.cons (env, subst, normalize_store prefix constr) Stream.nil
        )
  with Occurs_check -> Stream.cons st Stream.nil

let conj f g st = Stream.bind (f st) g

let (&&&) = conj

let disj f g st = Stream.mplus (f st) (g st)

let (|||) = disj

let rec (?|) = function
| [h]  -> h
| h::t -> h ||| ?| t

let rec (?&) = function
| [h]  -> h
| h::t -> h &&& ?& t

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

let success st = Stream.cons st Stream.nil
let failure _  = Stream.nil;;

@type ('a, 'l) llist = Nil | Cons of 'a * 'l with show, gmap(*, html, eq, compare, foldl, foldr *)
@type 'a lnat = O | S of 'a with show, html, eq, compare, foldl, foldr, gmap;;

module type T = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
module type T2 = sig
  type ('a, 'b) t
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

exception NotImplemented

module Fmap1 (T : T) = struct
  open T
  external fmap : ('a, 'c) fancy t -> ('a t, 'c t) fancy = "%identity"
end

module Fmap2 (T : T2) = struct
  type ('a,'b) t = ('a,'b) T.t
  external fmap : (('a, 'c) fancy, ('q, 'e) fancy) t ->
                  (('a, 'q) t, ('c, 'e) t) fancy = "%identity"
end



(* let lmap : ('a, 'b) fancy -> (('a, 'l) llist as 'l, ('b, 'm) llist as 'm) fancy = fun x -> Cons (x, Nil)

let cons : ('a, 'b logic) fancy -> (('a, 'z) llist as 'z, ('b logic, 'c) llist logic as 'c) fancy -> (('a, 'z) llist, ('b logic, 'c) llist logic) fancy = fun x y ->
  Cons (x, y)

let nil : (('a, 'z) llist as 'z, ('a logic, 'c) llist logic as 'c) fancy = Nil *)


module Bool =
  struct

    type 'a logic' = 'a logic
    let logic' = logic

    type ground = bool

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (GT.bool) n
          method eq      n m = GT.eq     (GT.bool) n m
          method compare n m = GT.compare(GT.bool) n m
          method foldr   n   = GT.foldr  (GT.bool) n
          method foldl   n   = GT.foldl  (GT.bool) n
          method gmap    n   = GT.gmap   (GT.bool) n
          method show    n   = GT.show   (GT.bool) n
        end
    }

    type logic = bool logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (logic') (GT.html   (ground)) n
          method eq      n m = GT.eq     (logic') (GT.eq     (ground)) n m
          method compare n m = GT.compare(logic') (GT.compare(ground)) n m
          method foldr   a n = GT.foldr  (logic') (GT.foldr  (ground)) a n
          method foldl   a n = GT.foldl  (logic') (GT.foldl  (ground)) a n
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (ground)) n
          method show    n   = GT.show   (logic') (GT.show   (ground)) n
        end
    }

    type boolf = (bool,bool) fancy
    type groundf = boolf
    type fancy   = groundf

    let false_ : boolf = inj@@lift false
    let true_  : boolf = inj@@lift true

    let (|^) a b c =
      conde [
        (a === false_) &&& (b === false_) &&& (c === true_);
        (a === false_) &&& (b === true_)  &&& (c === true_);
        (a === true_)  &&& (b === false_) &&& (c === true_);
        (a === true_)  &&& (b === true_)  &&& (c === false_);
      ]

    let noto' a na = (a |^ a) na

    let noto a = noto' a true_

    let oro a b c =
      Fresh.two (fun aa bb ->
        ((a  |^ a) aa) &&&
        ((b  |^ b) bb) &&&
        ((aa |^ bb) c)
      )

    let ando a b c =
      Fresh.one (fun ab ->
        ((a  |^ b) ab) &&&
        ((ab |^ ab) c)
      )

    let (&&) a b = ando a b true_
    let (||) a b = oro  a b true_

    let show_ground : boolf -> string = string_of_bool

    let inj b : boolf = inj@@lift b
  end

let eqo x y t =
  conde [
    (x === y) &&& (t === Bool.true_);
    (x =/= y) &&& (t === Bool.false_);
  ]

let neqo x y t =
  conde [
    (x =/= y) &&& (t === Bool.true_);
    (x === y) &&& (t === Bool.false_);
  ];;


module Nat =
  struct

    type 'a logic' = 'a logic
    let logic' = logic

    type 'a t = 'a lnat

    type ground = ground t

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n = GT.html   (lnat) this#html    n
          method eq      n = GT.eq     (lnat) this#eq      n
          method compare n = GT.compare(lnat) this#compare n
          method foldr   n = GT.foldr  (lnat) this#foldr   n
          method foldl   n = GT.foldl  (lnat) this#foldl   n
          method gmap    n = GT.gmap   (lnat) this#gmap    n
          method show    n = GT.show   (lnat) this#show    n
        end
    }

    type logic  = logic t logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (logic') (GT.html   (lnat) this#html   ) n
          method eq      n m = GT.eq     (logic') (GT.eq     (lnat) this#eq     ) n m
          method compare n m = GT.compare(logic') (GT.compare(lnat) this#compare) n m
          method foldr   a n = GT.foldr  (logic') (GT.foldr  (lnat) this#foldr  ) a n
          method foldl   a n = GT.foldl  (logic') (GT.foldl  (lnat) this#foldl  ) a n
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (lnat) this#gmap   ) n
          method show    n   = GT.show   (logic') (GT.show   (lnat) this#show   ) n
        end
    }

    let rec of_int n = if n <= 0 then O else S (of_int (n-1))
    let rec to_int   = function O -> 0 | S n -> 1 + to_int n

    let (!) = (!!)

    let rec inj' n : (_,_) fancy = inj @@ lift (GT.gmap(lnat) inj' n)
    let inj = inj'

    (* let prj_k k n =
      let rec inner n =
        GT.gmap(lnat) inner (prj_k k n)
      in
      inner n

    let prj n = prj_k (fun _ -> raise Not_a_value) n *)

    let o = inj@@lift O
    let s x = inj@@lift (S x)

    type groundf = (ground, ground) fancy

    let rec addo x y z =
      printf "addo: '%s' '%s' '%s'\n%!" (generic_show x) (generic_show y) (generic_show z);
      conde [
        (x === o) &&& (z === y);
        Fresh.two (fun x' z' ->
           (x === (s x')) &&&
           (z === (s z')) &&&
           (addo x' y z')
        )
      ]

    let (+) = addo

    let rec mulo x y z =
      printf "mulo: '%s' '%s' '%s'\n%!" (generic_show x) (generic_show y) (generic_show z);
      conde [
        (x === o) &&& (z === o);
        Fresh.two (fun x' z' ->
          (x === (s x')) &&&
          (addo y z' z) &&&
          (mulo x' y z')
        )
      ]

    let ( * ) = mulo

    let rec leo x y b =
      conde [
        (x === o) &&& (b === Bool.true_);
        Fresh.two (fun x' y' ->
          conde [
            (x === (s x')) &&& (y === (s y')) &&& (leo x' y' b)
          ]
        )
      ]

    let geo x y b = leo y x b

    let (<=) x y = leo x y Bool.true_
    let (>=) x y = geo x y Bool.false_

    let gto x y b = conde [(x >= y) &&& (x =/= y) &&& (b === Bool.true_)]
    let lto x y b = gto y x b

    let (>) x y = gto x y Bool.true_
    let (<) x y = lto x y Bool.true_

    let show_ground: (ground,ground) fancy -> string = GT.show(ground)

  end

let rec inj_nat n =
  if n <= 0 then inj O
  else inj (S (inj_nat @@ n-1))

(* let rec prj_nat n =
  match prj n with
  | O   -> 0
  | S n -> 1 + prj_nat n *)


let none () : ('a option,'b) fancy = None
let some x :  ('a option, 'b option) fancy = inj@@lift (Some x)

module List = struct

    include List

    type 'a logic' = 'a logic

    let logic' = logic

    type ('a, 'l) t = ('a, 'l) llist

    module MyFmap = struct
      type ('a,'b) t = ('a, 'b) llist
      let fmap f g = function
      | Nil -> Nil
      | Cons (x,xs) -> Cons (f x, g xs)
      (* TODO: rewrite *)
    end
    module F = Fmap2(MyFmap)

    let nil ()  = inj (F.fmap Nil)
    let cons x y = inj (F.fmap (Cons (x, y)))

    type 'a ground = ('a, 'a ground) t;;


    let rec of_list = function [] -> nil () | x::xs -> cons x (of_list xs)
    (* let rec to_list = function Nil -> [] | Cons (x, xs) -> x::to_list xs *)

    let (%)   = cons
    let (%<) x y = cons x @@ cons y @@ nil ()
    let (!<) x   = cons x @@ nil ()

    (* let rec inj fa l = !! (GT.gmap(llist) fa (inj fa) l)

    let prj_k fa k l =
      let rec inner l =
        GT.gmap(llist) fa inner (prj_k k l)
      in
      inner l

    let prj fa l = prj_k fa (fun _ -> raise Not_a_value) l *)

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          (* method html    fa l = GT.html   (llist) fa (this#html    fa) l *)
          (* method eq      fa l = GT.eq     (llist) fa (this#eq      fa) l
          method compare fa l = GT.compare(llist) fa (this#compare fa) l
          method foldr   fa l = GT.foldr  (llist) fa (this#foldr   fa) l
          method foldl   fa l = GT.foldl  (llist) fa (this#foldl   fa) l *)
          method gmap    fa l = GT.gmap   (llist) fa (this#gmap    fa) l
          method show    fa l = "[" ^
            let rec inner l =
              (GT.transform(llist)
                 (GT.lift fa)
                 (GT.lift inner)
                 (object inherit ['a,'a ground] @llist[show]
                    method c_Nil   _ _      = ""
                    method c_Cons  i s x xs = x.GT.fx () ^ (match xs.GT.x with Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                  end)
                 ()
                 l
              )
            in inner l ^ "]"
        end
    }

    type 'a logic  = ('a, 'a logic) t logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    fa l   = GT.gmap   (logic') (GT.gmap   (llist) fa (this#gmap    fa)) l
          method show : ('a -> string) -> 'a logic -> GT.string = fun fa l ->
            GT.show(logic')
              (fun l -> "[" ^
                 let rec inner l =
                    GT.transform(llist)
                      (GT.lift fa)
                      (GT.lift (GT.show(logic) inner))
                      (object inherit ['a,'a logic] @llist[show]
                         method c_Nil   _ _      = ""
                         method c_Cons  i s x xs =
                           x.GT.fx () ^ (match xs.GT.x with Value Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                       end)

                    () l
                   in inner l ^ "]"
              )
              l
        end
    }

    type ('a, 'b) flist = (('a, 'b) fancy ground, 'b list) fancy

    let rec foldro f a xs r =
      conde [
        (xs === nil ()) &&& (a === r);
        Fresh.three (fun h t a'->
            (xs === h % t) &&&
            (f h a' r) &&&
            (foldro f a t a')
        )
      ]

    let rec mapo f xs ys =
      conde [
        (xs === nil ()) &&& (ys === nil ());
        Fresh.two (
          fun z zs ->
            (xs === z % zs) &&&
            (Fresh.two (
               fun a1 a2 ->
                 (f z a1) &&&
                 (mapo f zs a2) &&&
                 (ys === a1 % a2)
            ))
        )
      ]

    let filtero p xs ys =
      let folder x a a' =
        conde [
          (p x Bool.true_) &&& (x % a === a');
          (p x Bool.false_) &&& (a === a')
        ]
      in
      foldro folder (nil ()) xs ys

    let rec lookupo p xs mx =
      conde [
        (xs === nil ()) &&& (mx === none ());
        Fresh.two (fun h t ->
          (h % t === xs) &&&
          (conde [
            (p h Bool.true_) &&& (mx === (some h));
            (p h Bool.false_) &&& (lookupo p t mx)
          ])
        )
      ]

    let anyo = foldro Bool.oro Bool.false_

    let allo = foldro Bool.ando Bool.true_

    let rec lengtho l n =
      conde [
        (l === nil ()) &&& (n === Nat.o);
        Fresh.three (fun x xs n' ->
          (l === x % xs)  &&&
          (n === (Nat.s n')) &&&
          (lengtho xs n')
        )
      ]

    let rec appendo a b ab =
      conde [
        (a === nil ()) &&& (b === ab);
        Fresh.three (fun h t ab' ->
	        (a === h%t) &&&
          (h%ab' === ab) &&&
          (appendo t b ab')
        )
      ]

    let rec reverso a b =
      conde [
        (a === nil ()) &&& (b === nil ());
        Fresh.three (fun h t a' ->
          (a === h%t) &&&
          (appendo a' !<h b) &&&
          (reverso t a')
        )
      ]

    let rec membero l a =
      Fresh.two (fun x xs ->
        (l === x % xs) &&&
        (conde [
           x === a;
           (x =/= a) &&& (membero xs a)
         ])
      )


    let nullo q : goal = (q === nil())

    let caro xs h  : goal = call_fresh (fun tl -> xs === (h % tl))
    let cdro xs tl : goal = call_fresh (fun h  -> xs === (h % tl))
    let hdo = caro
    let tlo = cdro

    let show : ('a -> string) -> (('a,'b) llist as 'b) -> string = fun f xs ->
      let b = Buffer.create 40 in
      let rec helper = function
      | Nil -> ()
      | Cons (a,Nil) -> bprintf b "%s " (f a)
      | Cons (a, xs) -> bprintf b "%s; " (f a); helper xs
      in
      bprintf b "[ ";
      helper xs;
      bprintf b "]";
      Buffer.contents b
  end

let (%)  = List.cons
let cons : ('a,_) fancy -> (('a,'b) llist as 'b, _) fancy -> (('a,'b) llist as 'b, _) fancy = List.cons
let (%<) = List.(%<)
let (!<) = List.(!<)
let nil  = List.nil

let rec inj_list: ('a,'b) fancy list -> (( ('a,'b) fancy,'c) llist as 'c, 'b list) fancy  = function
| []    -> nil ()
| x::xs -> cons x (inj_list xs)

let inj_list_p xs = inj_list @@ List.map (fun (x,y) -> inj_pair x y) xs

(* let (_:int) = inj_list *)
(* let rec prj_list l =
  match prj l with
  | Nil -> []
  | Cons (x, xs) -> prj x :: prj_list xs *)





let rec inj_nat_list = function
| []    -> nil()
| x::xs -> inj_nat x % inj_nat_list xs
(*
let rec prj_nat_list l =
  match prj l with
  | Nil -> []
  | Cons (x, xs) -> prj_nat x :: prj_nat_list xs
*)

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
  try walk (Obj.repr x); false
  with FreeVarFound -> true

(* let (_:int) = has_free_vars *)

exception WithFreeVars of (Obj.t -> bool) * Obj.t

let rec refine : State.t -> ('a, 'c) fancy -> ('a,'c) fancy = fun ((e, s, c) as st) x ->
  let rec walk' recursive env var subst =
    (* let () = printf "walk' for var = '%s'\n%!" (generic_show var) in *)
    let var = Subst.walk env var subst in
    match Env.var env var with
    | None ->
        (match wrap (Obj.repr var) with
         | Unboxed _ -> !!!var
         | Boxed (t, s, f) ->
            let var = Obj.dup (Obj.repr var) in
            let sf =
              if t = Obj.double_array_tag
              then !!! Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to s - 1 do
              sf var i (!!!(walk' true env (!!!(f i)) subst))
           done;
           (Obj.magic var)
         | Invalid n -> invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i when recursive -> var
      (* invalid_arg "Free variable in refine." *)
(*
        (match var with
         | Var (a, i, _) ->
            let cs =
	      List.fold_left
		(fun acc s ->
		   match walk' false env (!!!var) s with
		   | Var (_, j, _) when i = j -> acc
		   | t -> (refine st t) :: acc
		)
		[]
		c
	    in
	    Var (a, i, cs)
        )
*)
    | _ -> (Obj.magic var)
  in
  !!!(walk' true e (!!!x) s)

(* let (_:int) = refine *)

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module ApplyTuple =
  struct
    let one arg x = x arg

    let succ prev = fun arg (x, y) -> (x arg, prev arg y)
  end

module ApplyLatest =
  struct
    let two = (ApplyTuple.one, ExtractDeepest.ext2)

    let apply (appf, extf) tup =
      let x, base = extf tup in
      appf base x

    let succ (appf, extf) = (ApplyTuple.succ appf, ExtractDeepest.succ extf)
  end

module Uncurry =
  struct
    let succ k f (x,y) = k (f x) y
  end

type ('a,'b) reification_rez = Final of 'b | HasFreeVars of ((Obj.t -> bool) * Obj.t)
type ('a,'b) refiner = State.t Stream.t -> ('a, 'b) reification_rez Stream.t

let refiner : ('a, 'b) fancy -> ('a, 'b) refiner = fun x ans ->
  let f ((e,_,_) as st) =
    let ans(*: ('a, 'b) fancy *) = refine st x in
    let isVar : Obj.t -> bool = Env.is_var e in
    if has_free_vars isVar !!!ans then HasFreeVars (isVar, Obj.repr ans)
    else Final !!!ans
  in
  Stream.map f ans

let (_: (int, int list) refiner) = refiner ((!!!5) : (int,int list) fancy)

module LogicAdder =
  struct
    let zero f = f

    let succ (prev: 'a -> State.t -> 'b) (f: ('c, 'c2) fancy -> 'a) : State.t -> ('c, 'c2) refiner * 'b =
      call_fresh (fun logic st -> (refiner logic, prev (f logic) st))
  end

let one () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyLatest.two

let succ n () =
  let adder, currier, app = n () in
  (LogicAdder.succ adder, Uncurry.succ currier, ApplyLatest.succ app)

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
  let run f = f (State.empty ()) in
  run (adder goalish) |> ApplyLatest.apply app_num |> (currier f)

let (_:(int,int) fancy) = inj@@lift 1
(* let (_:int) = run one (fun q -> q === inj@@lift 1) *)
