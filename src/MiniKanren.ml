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
      | Nil, _      | _, Nil       -> failwith "MiniKanren.Stream.zip: streams have different lengths"

  end

let (!!!) = Obj.magic

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
    | Invalid n                                         -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed s when Obj.(string_tag = (tag @@ repr s)) -> bprintf b "\"%s\"" (!!!s)
    | Unboxed n when !!!n = 0                           -> Buffer.add_string b "[]"
    | Unboxed n                                         -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!!n))
    | Boxed  (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b
;;

@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

(* miniKanren-related stuff starts here *)
type ('a, 'b) injected = 'a

external lift: 'a -> ('a, 'a) injected = "%identity"
external inj: ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"
;;

(* The [token_t] type is use to connect logic variables with environment where they were created *)
@type token_env = GT.int with show,gmap,html,eq,compare,foldl,foldr;;

(* Global token will not be exported outside and will be used to detect the value
 * was actually created by us *)
type token_mk = int list
let global_token: token_mk = [8];;

type inner_logic =
| InnerVar of token_mk * token_env * int * Obj.t list

let (!!!) = Obj.magic

let rec bprintf_logic: Buffer.t -> ('a -> unit) -> 'a logic -> unit = fun b f x ->
  let rec helper = function
  | Value x -> f x
  | Var (i,cs) ->
    bprintf b " _.%d" i;
    List.iter (fun x -> bprintf b "=/= "; helper x) cs
  in
  helper x

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
            method c_Var _ s i cs =
              (* I have some issues with callign show_logic there, so copy-paste*)
              (* show_logic (fun _ -> assert false) (Var(_token,i,cs)) *)
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
};;

let (!!) x = inj (lift x)

let inj_pair : ('a, 'c) injected -> ('b,'d) injected -> ('a * 'b, ('c * 'd) logic) injected =
  fun x y -> (x, y)

external inj_int : int -> (int, int logic) injected = "%identity"

exception Not_a_value
exception Occurs_check

module Int = struct type t = int let compare : int -> int -> int = Pervasives.compare end
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
    type t = { token : token_env;
               mutable next: int;
               (* mutable reifiers: Obj.t MultiIntMap.t; *)
              }

    let last_token : token_env ref = ref 0
    let empty () =
      incr last_token;
      { token= !last_token; next=10 }

    let fresh e =
      let v = InnerVar (global_token, e.token, e.next, []) in
      e.next <- 1+e.next;
      (* printf "new fresh var with index=%d\n" e.next; *)
      (!!!v, e)

    let var_tag, var_size =
      let dummy_index = 0 in
      let dummy_token = 0 in
      let v = InnerVar (global_token, dummy_token, dummy_index, []) in
      Obj.tag (!!! v), Obj.size (!!! v)

    let var {token=env_token;_} x =
      (* There we detect if x is a logic variable and then that it belongs to current env *)
      let t = !!! x in
      if Obj.tag  t = var_tag  &&
         Obj.size t = var_size &&
         (let q = Obj.field t 0 in
          (Obj.is_block q) && q == (!!!global_token)
         )
      then (let q = Obj.field t 1 in
            if (Obj.is_int q) && q == (!!!env_token)
            then let InnerVar (_,_,i,_) = !!! x in Some i
            else failwith "You hacked everything and pass logic variables into wrong environment"
            )
      else None

    let is_var env v = None <> var env v

    (* Some tests for to check environment self-correctness *)
    (*
    let () =
      let e1 = empty () in
      let e2 = empty () in
      assert (e1 != e2);
      assert (e1.token != e2.token);
      let (q1,e11) = fresh e1 in
      assert (is_var e1  q1);
      assert (is_var e11 q1);
      assert (is_var e2  q1);
      ()
    *)
  end

module Subst :
  sig
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

    (* map from var indicies to tuples of (actual vars, value) *)
    type content = { lvar: Obj.t; new_val: Obj.t }
    type t = content M.t
    let new_val {new_val=x;_} = Obj.obj x
    let lvar    {lvar=v;_}    = Obj.obj v
    let make_content a b = { lvar=Obj.repr a; new_val=Obj.repr b }

    let show m =
      let b = Buffer.create 40 in
      Buffer.add_string b "subst {";
      M.iter (fun i {new_val} -> bprintf b "%d -> %s; " i (generic_show new_val)) m;
      Buffer.add_string b "}";
      Buffer.contents b

    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, cnt) -> M.add i cnt s) empty l

    let split s = M.fold (fun _ {lvar;new_val} (xs, ts) -> (lvar::xs, new_val::ts)) s ([], [])

    let rec walk : Env.t -> 'a -> t -> 'a = fun env var subst ->
      match Env.var env !!!var with
      | None   -> var
      | Some i ->
          try walk env (new_val @@ M.find i subst) subst
          with Not_found -> var

    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
         match wy with
         | Invalid n when n = Obj.closure_tag -> false
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

type 'a goal' = State.t -> 'a
type goal = State.t Stream.t goal'

let call_fresh f (env, subst, constr)  =
  let x, env' = Env.fresh env in
  f x (env', subst, constr)

exception Disequality_violated

let (===) (x: _ injected) y (env, subst, constr) =
  (* we should always unify two injected types *)

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
    let subst  = Subst.of_list prefix in
    let prefix = List.split (List.map Subst.(fun (_, {lvar;new_val}) -> (lvar, new_val)) prefix) in
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
        | _  ->
          let new_constrs = normalize_store prefix constr in
          Stream.cons (env, subst, new_constrs) Stream.nil
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

exception WithFreeVars of (Obj.t -> bool) * Obj.t

let rec refine : State.t -> ('a,'b) injected -> ('a,'b) injected = fun ((e, s, c) as st) x ->
  let rec walk' recursive env var subst =
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
           !!!var
         | Invalid n -> invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i when recursive ->
        (match var with
        | InnerVar (token1, token2, i, _) ->
            (* We do not add extra Value here: they will be added on manual reification stage *)
            let cs =
              List.fold_left (fun acc s ->
                match walk' false env (!!!var) s with
                | maybeVar when Some i = Env.var env maybeVar -> acc
                | t -> (!!!(refine st !!!t)) :: acc
                )
                []
                c
            in
            Obj.magic @@ InnerVar (token1, token2, i, cs)
        )
    | _ -> var
  in
  !!!(walk' true e (!!!x) s)

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end


type helper = < isVar : 'a . 'a -> bool >

exception HasFreeVariables

(* TODO: rename reification_rez to good name *)
class type ['a,'b] reification_rez = object
  method has_free: bool
  method as_plain_exn: 'a
  method as_logic: (helper -> ('a, 'b) injected -> 'b) -> inj:('a -> 'b) -> 'b
end

let make_rr : ('a, 'b) injected -> State.t -> ('a, 'b) reification_rez = fun x st ->
  let ans = refine st x in
  let has_free = has_free_vars (Env.is_var @@ State.env st) (Obj.repr ans) in
  let c: helper = !!!(object method isVar x = Env.is_var (State.env st) (Obj.repr x) end) in
  object(self)
    method has_free = has_free
    method as_plain_exn = if self#has_free then raise HasFreeVariables else !!!ans
    method as_logic refiner ~inj =
      if self#has_free then refiner c ans else inj ans
  end

module R : sig
  type ('a, 'b) refiner

  val refiner :  ('a, 'b) injected -> ('a, 'b) refiner

  val apply_refiner : State.t Stream.t -> ('a, 'b) refiner -> ('a, 'b) reification_rez Stream.t
end = struct
  type ('a, 'b) refiner = State.t Stream.t -> ('a, 'b) reification_rez Stream.t

  let refiner : ('a,'b) injected -> ('a,'b) refiner = fun x -> Stream.map (make_rr x)
  let apply_refiner = fun st r -> r st
end

module ApplyTuple =
  struct
    let one arg r = R.apply_refiner arg r

    let succ prev = fun arg (r, y) -> (R.apply_refiner arg r, prev arg y)
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

type ('a, 'b) refiner = ('a, 'b) R.refiner
let refiner = R.refiner

module LogicAdder :
  sig
    val zero : 'a -> 'a
    val succ: ('a -> State.t -> 'd) -> (('e, 'f) injected -> 'a) -> State.t -> ('e, 'f) R.refiner * 'd
  end = struct
    let zero f = f

    let succ prev f =
      call_fresh (fun logic st -> (R.refiner logic, prev (f logic) st))
  end

let one () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyLatest.two

let succ n () =
  let adder, currier, app = n () in
  (LogicAdder.succ adder, Uncurry.succ currier, ApplyLatest.succ app)

let succ = (*!!!*)succ
let one = (*!!!*)one
let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = (*!!!*)one
let qr    = (*!!!*)two
let qrs   = (*!!!*)three
let qrst  = (*!!!*)four
let pqrst = (*!!!*)five

let run n goalish f =
  let adder, currier, app_num = n () in
  let run f = f (State.empty ()) in
  run (adder goalish) |> ApplyLatest.apply app_num |> (currier f)

let delay : (unit -> goal) -> goal = fun g ->
  fun st -> Stream.from_fun (fun () -> g () st)

(* ************************************************************************** *)
module type T1 = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
module type T2 = sig
  type ('a, 'b) t
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end
module type T3 = sig
  type ('a, 'b, 'c) t
  val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
end
module type T4 = sig
  type ('a, 'b, 'c, 'd) t
  val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
end
module type T5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
end
module type T6 = sig
  type ('a, 'b, 'c, 'd, 'e, 'f) t
  val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
end

let var_of_injected_exn : helper -> ('a,'b) injected -> (helper -> ('a,'b) injected -> 'b) -> 'b = fun c x r ->
  if c#isVar x
  then let InnerVar (_,_,n,cstr) = !!!x in !!!(Var (n, List.map (!!!(r c)) !!!cstr))
  else failwith "Bad argument of var_of_injected: it should be logic variable"

module Fmap1 (T : T1) = struct
  external distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected = "%identity"

  let rec reify: (helper -> ('a,'b) injected -> 'b) -> helper ->
        ('a T.t, 'b T.t logic as 'r) injected -> 'r
    = fun arg_r c x ->
      if c#isVar x
      then var_of_injected_exn c x (reify arg_r)
      else Value (T.fmap (arg_r c) x)
end

module Fmap2 (T : T2) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected = "%identity"

  let rec reify:
        (helper -> ('a,'b) injected -> 'b) ->
        (helper -> ('c,'d) injected -> 'd) ->
         helper -> (('a,'c) T.t, ('b,'d) T.t logic) injected -> ('b,'d) T.t logic
    = fun r1 r2 c x ->
      if c#isVar x then var_of_injected_exn c x (reify r1 r2)
      else Value (T.fmap (r1 c) (r2 c) x)
end

module Fmap3 (T : T3) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) T.t
  external distrib : (('a, 'b) injected, ('c, 'd) injected, ('e, 'f) injected) t -> (('a, 'c, 'e) t, ('b, 'd, 'f) t) injected = "%identity"

  let rec reify r1 r2 r3 (c: helper) x =
    if c#isVar x then var_of_injected_exn c x (reify r1 r2 r3)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) x)
end

module Fmap4 (T : T4) = struct
  type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) T.t
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) t ->
                     (('a, 'c, 'e, 'g) t, ('b, 'd, 'f, 'h) t) injected = "%identity"

  let rec reify r1 r2 r3 r4 (c: helper) x =
    if c#isVar x then var_of_injected_exn c x (reify r1 r2 r3 r4)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) (r4 c) x)
end

module Fmap5 (T : T5) = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) T.t
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) t ->
                     (('a, 'c, 'e, 'g, 'i) t, ('b, 'd, 'f, 'h, 'j) t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 (c: helper) x =
    if c#isVar x then var_of_injected_exn c x (reify r1 r2 r3 r4 r5)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) (r4 c) (r5 c) x)
end

module Fmap6 (T : T6) = struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, 'b, 'c, 'd, 'e, 'f) T.t
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) t ->
                     (('a, 'c, 'e, 'g, 'i, 'k) t, ('b, 'd, 'f, 'h, 'j, 'l) t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 r6 (c: helper) x =
    if c#isVar x then var_of_injected_exn c x (reify r1 r2 r3 r4 r5 r6)
    else Value (T.fmap (r1 c) (r2 c) (r3 c) (r4 c) (r5 c) (r6 c) x)
end

module Pair = struct
  module X = struct
    type ('a,'b) t = 'a * 'b
    let fmap f g (x,y) = (f x, g y)
  end
  include Fmap2(X)
end

module ManualReifiers = struct
  let rec simple_reifier: helper -> ('a, 'a logic) injected -> 'a logic = fun c n ->
    if c#isVar n
    then var_of_injected_exn c n simple_reifier
    else Value  n

  let bool_reifier : helper -> (bool, bool logic) injected -> bool logic =
    simple_reifier

  let rec int_reifier: helper -> (int, int logic) injected -> int logic = fun c n ->
    if c#isVar n
    then var_of_injected_exn c n int_reifier
    else Value n

  let rec string_reifier: helper -> (string, string logic) injected -> string logic = fun c x ->
    if c#isVar x
    then var_of_injected_exn c x string_reifier
    else Value  x

  let rec pair_reifier: (helper -> ('a,'b) injected -> 'b) ->
                        (helper -> ('c,'d) injected -> 'd) ->
                        helper -> ('a * 'c, ('b * 'd) logic as 'r) injected -> 'r
    = fun r1 r2 c p ->
      if c#isVar p then var_of_injected_exn c p (pair_reifier r1 r2)
      else Pair.reify r1 r2 c p

end;;

(* ***************************** a la relational StdLib here ***************  *)
@type ('a, 'l) llist = Nil | Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr;;
@type 'a lnat = O | S of 'a with show, html, eq, compare, foldl, foldr, gmap;;

let none () = inj@@lift None
let some x  = inj@@lift (Some x)

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

    type boolf = (bool, bool logic') injected
    type groundi = boolf
    type injected   = groundi

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

    let show_ground  : ground  -> string = string_of_bool

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

module Nat = struct
    type 'a logic' = 'a logic
    let logic' = logic

    module X = struct
      type 'a t = 'a lnat
      let fmap f = function
      | O -> O
      | S n -> S (f n)
    end
    include X
    module F = Fmap1(X)

    type ground = ground t
    type logic = logic t logic'
    type groundi = (ground, logic) injected

    let rec reify : helper -> (ground, logic) injected -> logic  = fun c x ->
      if c#isVar x then var_of_injected_exn c x reify
      else F.reify reify c x

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

    let o = inj@@lift O
    let s x = inj@@lift (S x)

    let rec addo x y z =
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
      conde [
        ((x === o)) &&& (z === o);
        Fresh.two (fun x' z' ->
          (x === (s x')) &&& (
          (addo y z' z) &&&
          (mulo x' y z'))
        )
      ]

    let ( * ) = mulo

    let rec leo x y b =
      conde [
        (x === o) &&& (b === Bool.true_);
        (x =/= o) &&& (y === o) &&& (b === Bool.false_);
        Fresh.two (fun x' y' ->
          (x === (s x')) &&& (y === (s y')) &&& (leo x' y' b)
        )
      ]

    let geo x y b = leo y x b

    let (<=) x y = leo x y Bool.true_
    let (>=) x y = geo x y Bool.false_

    let rec gto x y b = conde
      [ (x =/= o) &&& (y === o) &&& (b === Bool.true_)
      ; (x === o) &&& (b === Bool.false_)
      ; Fresh.two (fun x' y' ->
          (x === s x') &&& (y === s y') &&& (gto x' y' b)
        )
      ]

    let lto x y b = gto y x b

    let (>) x y = gto x y Bool.true_
    let (<) x y = lto x y Bool.true_

    let show_ground:  ground  -> string = GT.show(ground)
end

let rec inj_nat n =
  if n <= 0 then inj  O
  else inj  (S (inj_nat @@ n-1))

module List =
  struct

    include List

    type 'a logic' = 'a logic

    let logic' = logic

    type ('a, 'l) t = ('a, 'l) llist

    module X = struct
      type ('a,'b) t = ('a, 'b) llist
      let fmap f g = function
      | Nil -> Nil
      | Cons (x,xs) -> Cons (f x, g xs)
    end
    module F = Fmap2(X)

    let nil ()  = inj (F.distrib Nil)
    let cons x y = inj (F.distrib (Cons (x, y)))

    type 'a ground = ('a, 'a ground) t;;
    type 'a logic  = ('a, 'a logic) t logic'

    let rec reify : _ -> helper -> (_ ground, 'b logic) injected -> 'b logic = fun arg_r c x ->
      if c#isVar x then var_of_injected_exn c x (reify arg_r)
      else F.reify arg_r (reify arg_r) c x

    let rec of_list = function [] -> nil () | x::xs -> cons x (of_list xs)

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    fa l = GT.html   (llist) fa (this#html    fa) l
          method eq      fa l = GT.eq     (llist) fa (this#eq      fa) l
          method compare fa l = GT.compare(llist) fa (this#compare fa) l
          method foldr   fa l = GT.foldr  (llist) fa (this#foldr   fa) l
          method foldl   fa l = GT.foldl  (llist) fa (this#foldl   fa) l
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

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method compare fa l = GT.compare (logic') (GT.compare (llist) fa (this#compare fa)) l
          method gmap    fa l = GT.gmap    (logic') (GT.gmap    (llist) fa (this#gmap    fa)) l
          method eq      fa l = GT.eq      (logic') (GT.eq      (llist) fa (this#eq      fa)) l
          method foldl   fa l = GT.foldl   (logic') (GT.foldl   (llist) fa (this#foldl   fa)) l
          method foldr   fa l = GT.foldr   (logic') (GT.foldr   (llist) fa (this#foldr   fa)) l
          method html    fa l = GT.html    (logic') (GT.html    (llist) fa (this#html    fa)) l

          (* We override default implementation to show list as semicolon-separated *)
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

    type ('a,'b) groundi = ('a ground, 'b logic) injected

    let groundi =
      { GT.gcata = ()
      ; plugins = object
          method show : ('a -> string) -> ('a,_) groundi -> string = fun fa l ->
          (* we expect no free variables here *)
          GT.show(ground) fa (Obj.magic l : 'a ground)
        end
      }

    let (%): ('a,'b) injected -> ('a,'b) groundi -> ('a,'b) groundi = cons
    let (%<): ('a,'b) injected -> ('a,'b) injected -> ('a,'b) groundi = fun x y -> cons x @@ cons y @@ nil ()
    let (!<) : ('a,'b) injected -> ('a,'b) groundi = fun x -> cons x @@ nil ()

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
        Fresh.two (fun z zs ->
          (xs === z % zs) &&&
          (Fresh.two (fun a1 a2 ->
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

  end

let (%)  = List.cons
let (%<) = List.(%<)
let (!<) = List.(!<)
let nil  = List.nil

let rec inj_list: ('a, 'b) injected list -> ('a, 'b) List.groundi = function
| []    -> nil ()
| x::xs -> List.cons x (inj_list xs)

let inj_list_p xs = inj_list @@ List.map (fun (x,y) -> inj_pair x y) xs

let rec inj_nat_list = function
| []    -> nil()
| x::xs -> inj_nat x % inj_nat_list xs
