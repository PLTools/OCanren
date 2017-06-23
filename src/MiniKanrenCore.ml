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

let printfn fmt = kprintf (printf "%s\n%!") fmt
let (!!!) = Obj.magic


type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let is_valid_tag t =
  let open Obj in
  not (List.mem t
    [lazy_tag; closure_tag; object_tag; infix_tag; forward_tag; no_scan_tag;
     abstract_tag; custom_tag; custom_tag; unaligned_tag; out_of_heap_tag])

let rec wrap (x : Obj.t) =
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

let generic_show ?(maxdepth= 99999) x =
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
;;

let list_filter_map ~f xs =
  let rec helper acc = function
  | [] -> List.rev acc
  | x::xs -> begin
      match f x with None -> helper acc xs | Some y -> helper (y::acc) xs
    end
  in
  helper [] xs


(* miniKanren-like streams, the most unsafe implementation *)
module MKStream =
  struct
    open Obj
    (*
      Very unsafe implementation of streams
      * false -- an empty list
      * closure -- delayed list
      * block with tag 1 -- single value
      * (x,closure)   -- a value and continuation (pair has tag 0)
    *)

    type t = Obj.t

    let nil : t = !!!false
    let is_nil s = (s = !!!false)

    let inc (f: unit -> t) : t =
      Obj.repr f

    let from_fun = inc

    type wtf = Dummy of int*string | Single of Obj.t
    let () = assert (Obj.tag @@ repr (Single !!![]) = 1)

    let single : 'a -> t = fun x ->
      Obj.repr @@ Obj.magic (Single !!!x)

    let choice a f =
      assert (closure_tag = tag@@repr f);
      Obj.repr @@ Obj.magic (a,f)

    let case_inf xs ~f1 ~f2 ~f3 ~f4 : Obj.t =
      if is_int xs then f1 ()
      else
        let tag = Obj.tag (repr xs) in
        if tag = Obj.closure_tag
        then f2 (!!!xs: unit -> Obj.t)
        else if tag = 1 then f3 (field (repr xs) 0)
        else
          (* let () = assert (0 = tag) in
          let () = assert (2 = size (repr xs)) in *)
          f4 (field (repr xs) 0) (!!!(field (repr xs) 1): unit -> Obj.t)
      (* [@@inline ] *)

    let step gs =
      assert (closure_tag = tag @@ repr gs);
      !!!gs ()

    let rec mplus : t -> t -> t  = fun cinf (gs: t) ->
      assert (closure_tag = tag @@ repr gs);
      case_inf cinf
        ~f1:(fun () ->
              step gs)
        ~f2:(fun f ->
              inc begin fun () ->
                let r = step gs in
                mplus r !!!f
              end)
        ~f3:(fun c ->
              choice c gs
          )
        ~f4:(fun c ff ->
              choice c (inc @@ fun () -> mplus (step gs) !!!ff)
          )

    let rec bind cinf g =
      case_inf cinf
        ~f1:(fun () ->
                nil)
        ~f2:(fun f ->
              (* delay here because miniKanren has it *)
              inc begin fun () ->
                let r = f () in
                bind r g
              end)
        ~f3:(fun c ->
              (!!!g c) )
        ~f4:(fun c f ->
              let arg1 = !!!g c in
              mplus arg1 @@
                    inc begin fun () ->
                      bind (step f) g
                    end
          )
  end

module Stream =
  struct
    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec of_mkstream : MKStream.t -> 'a t = fun xs ->
      let rec helper xs =
        !!!MKStream.case_inf !!!xs
          ~f1:(fun () -> !!!Nil)
          ~f2:(fun f  ->
              !!! (from_fun (fun () ->
                helper @@ f ())) )
          ~f3:(fun a -> !!!(cons a Nil) )
          ~f4:(fun a f -> !!!(cons a @@ from_fun (fun () -> helper @@ f ())) )
      in
      !!!(helper !!!xs)

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

    (* let rec mplus fs gs =
      match fs with
      | Nil           -> gs
      | Cons (hd, tl) -> cons hd @@ from_fun (fun () -> mplus gs tl)
      | Lazy z        -> from_fun (fun () -> mplus gs (Lazy.force z) )

    let rec bind xs f =
      match xs with
      | Cons (x, xs) -> from_fun (fun () -> mplus (f x) (bind xs f))
      | Nil          -> nil
      | Lazy z       -> from_fun (fun () -> bind (Lazy.force z) f) *)


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
;;

(* ************************************************ *)
@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

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
}
;;
(* miniKanren-related stuff starts here *)

(* The [token_t] type is use to connect logic variables with environment where they were created *)
@type token_env = GT.int;;

(* Scope there are just ints but in faster-MK they use reference equality *)
type scope_t = int
let non_local_scope : scope_t = -6

let new_scope : unit -> scope_t =
  let scope = ref 0 in (* TODO: maybe put this into Env.t *)
  fun () -> (incr scope; !scope)

(* Global token will not be exported outside and will be used to detect the value
 * was actually created by us *)
type token_mk = int list
let global_token: token_mk = [-8];;

type inner_logic =
  { token_mk: token_mk; token_env: token_env; index: int
  (* set-var-val! stuff *)
  ; mutable subst: Obj.t option; scope: scope_t
  (* in the substitution we will store pair the same as in subst *)
  ; constraints: Obj.t list
  }

let make_inner_logic ~envt ~scope index = { token_env = envt; token_mk = global_token
  ; index; subst = None; scope; constraints = [] }

let is_inner_unbound x = (x.subst = None)
let subst_inner_term lo term = (lo.subst <- Some term)

let scope_of_inner { scope; _ } : scope_t = scope
let scope_eq (a: scope_t) (b: scope_t) = (compare a b = 0)

type ('a, 'b) injected = 'a

external lift: 'a -> ('a, 'a) injected = "%identity"
external inj: ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"

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

type helper = < isVar : 'a . 'a -> bool >

let var_of_injected_exn : helper -> ('a,'b) injected -> (helper -> ('a,'b) injected -> 'b) -> 'b = fun c x r ->
  if c#isVar x
  (* TODO: maybe create one more isVar with type : 'a -> inner_logic option *)
  then
    let x : inner_logic = !!!x in
    !!!(Var (x.index, List.map (!!!(r c)) x.constraints))
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

let rec simple_reifier: helper -> ('a, 'a logic) injected -> 'a logic = fun c n ->
  if c#isVar n
  then var_of_injected_exn c n simple_reifier
  else Value n

(** Importand part about reification and injected values finishes*)






exception Not_a_value
exception Occurs_check

let to_logic x = Value x

let from_logic = function
  | Value x    -> x
  | Var (_, _) -> raise Not_a_value

let (!!) x = inj (lift x)

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
end;;

let pretty_generic_show ?(maxdepth= 99999) is_var x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner depth term =
    if depth > maxdepth then Buffer.add_string b "..." else
    if is_var !!!term then begin
      let var = (!!!term : inner_logic) in
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
;;

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : ?name:string -> scope:scope_t -> t -> 'a * t
    val var    : t -> 'a -> int option
    val is_var : t -> 'a -> bool
  end =
  struct
    type t =  { token : token_env;
                mutable next: int;
              }

    let last_token : token_env ref = ref 11
    let empty () =
      incr last_token;
      { token= !last_token; next=10 }

    let fresh ?name ~scope e =
      let v = !!!(make_inner_logic ~envt:e.token ~scope e.next) in
      e.next <- 1+e.next;
      (!!!v, e)

    let var_tag, var_size =
      let index = 0 in (* dummy index *)
      let envt  = 0 in (* dummy env token *)
      let scope = 0 in (* dummy scope *)
      let v = make_inner_logic ~envt ~scope index in
      Obj.tag (!!! v), Obj.size (!!! v)

    let var env x =
      (* There we detect if x is a logic variable and then that it belongs to current env *)
      let t = !!! x in
      if Obj.tag  t = var_tag  &&
         Obj.size t = var_size &&
         (let token = (!!!x : inner_logic).token_mk in
          (Obj.is_block !!!token) && token == (!!!global_token)
         )
      then (let q = (!!!x : inner_logic).token_env in
            if (Obj.is_int !!!q) && q == (!!!env.token)
            then Some (!!!x : inner_logic).index
            else failwith "You hacked everything and pass logic variables into wrong environment"
            )
      else None

    let is_var env v = None <> var env v

    (* Some tests for to check environment self-correctness *)
(*
    let () =
      let scope = new_scope () in
      let e1 = empty () in
      let e2 = empty () in
      assert (e1 != e2);
      assert (e1.token != e2.token);
      let (q1,e11) = fresh ~scope e1 in
      let (q2,___) = fresh ~scope e11 in
      assert (is_var e1  q1);
      assert (is_var e11 q1);
      printfn "q1 is '%s'" (generic_show q1);
      printfn "q2 is '%s'" (generic_show q2);
      printfn "updating q1";
      (!!!q1 : inner_logic).subst <- Obj.repr "asdf";
      printfn "q1 is '%s'" (generic_show q1);
      printfn "q2 is '%s'" (generic_show q2);
      assert (is_var e2  q1);
      () *)
  end


module Subst :
  sig
    type t

    val empty   : t

    type content = { lvar: inner_logic; new_val: Obj.t }
    val make_content : inner_logic -> 'b  -> content

    val of_list : content list -> t

    (* splits substitution into two lists of the same length. 1st contains logic vars,
     * second values to substitute *)
    val split   : t -> inner_logic list * Obj.t list
    val walk    : Env.t -> 'a -> t -> 'a

    (* [merge_a_prefix_unsafe ~scope p s] unions prefix [p] and substitution [s].
      Very naive approach: no any walking or occurs_check is performed *)
    val merge_a_prefix_unsafe : scope:scope_t -> content list  -> t -> t
    (* Safe version of [merge_a_prefix_unsafe]. it can fail *)
    val merge_a_prefix: Env.t -> scope:scope_t -> content list -> t -> (t * bool) option

    val unify   : Env.t -> 'a -> 'a -> scope:scope_t -> t -> (content list * t) option
    val show    : t -> string
    val pretty_show : ('a -> bool) -> t -> string
  end = struct

    module M = Map.Make (Int)

    (* map from var indicies to tuples of (actual vars, value) *)
    type content = { lvar: inner_logic; new_val: Obj.t }
    type t = content M.t

    let new_val {new_val=x;_} = Obj.obj x
    let lvar    {lvar=v;_}    = v
    let make_content lvar b = { lvar; new_val=Obj.repr b }

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

    let empty = M.empty

    let of_list ts = List.fold_left (fun s cnt -> M.add cnt.lvar.index cnt s) M.empty ts

    let split s =
      M.fold (fun _ {lvar;new_val} (xs, ts) -> (lvar::xs, new_val::ts)) s ([], [])

    let subst_lookup_exn ui (u: inner_logic) map : content =
      match u.subst with
      | Some t -> {lvar=u; new_val=t}
      | None   ->  M.find !!!ui map

    let rec walk : Env.t -> 'a -> t -> 'a = fun env term subst ->
      let rec helper x =
        if Env.is_var env x
        then begin
          let v = (!!!x : inner_logic) in
          match v.subst with
          | Some term -> walk env !!!term subst
          | None ->
              try walk env (new_val @@ subst_lookup_exn v.index !!!v subst) subst
              with Not_found -> x
        end else
          term
      in
      helper term

    (* let walk_by_func env var lookupf =
      let rec helper var =
        match Env.var env !!!var with
        | None -> var
        | Some i ->
            try let ans = (lookupf i !!!var) in
                if ans != !!!var then helper ans else var
            with Not_found -> var
      in
      helper var

    let occurs_by_func env xi term lookupf =
      let rec helper term =
        let y = walk_by_func env term lookupf in
        match Env.var env y with
        | Some yi -> xi = yi
        | None ->
           match wrap (Obj.repr y) with
           | Invalid n when n = Obj.closure_tag -> false
           | Unboxed _ -> false
           | Invalid n -> invalid_arg (sprintf "Invalid value in occurs check (%d)" n)
           | Boxed (_, s, f) ->
              let rec inner i =
                if i >= s then false
                else helper (!!!(f i)) || inner (i+1)
              in
              inner 0
      in
      helper term *)

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

    let merge_a_prefix_unsafe ~scope prefix subst =
      ListLabels.fold_left prefix ~init:subst ~f:(fun acc cnt ->
        if scope_eq scope cnt.lvar.scope
        then  let () = subst_inner_term cnt.lvar cnt.new_val in
              acc
        else M.add cnt.lvar.index cnt acc
      )

    let unify env x y ~scope main_subst =
      (* The idea is to do unification and collect unification prefix during the process.
        RIGHT:
          It is safe to modify variables on the go. Beause two cases:
          * if we do unification just after conde, then the scope is already incremented and nothing goes into
            the fresh variables.
          * if we do unification after the fresh, then if case of failure unification it doesn't matter
            that variable will be distructively substituted: we will not look on these variables in future.
        *)

      let extend xi x term (prefix,sub1) =
        if occurs env xi term sub1 then raise Occurs_check
        else
          let cnt = make_content x term in
          assert (Env.var env x <> Env.var env term);
          (* It's safe to do destructive substitution here. See comment on the top *)
          let sub2 = merge_a_prefix_unsafe ~scope [cnt] sub1 in
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
                 | _, Invalid n -> invalid_arg (sprintf "Invalid values for unification (%d)" n)
                 | _ -> None
                )
      in
      try helper !!!x !!!y (Some ([], main_subst))
      with Occurs_check -> None

    let merge_a_prefix env ~scope prefix subst =
      let rec helper is_enlarged acc = function
      | [] -> Some (acc, is_enlarged)
      | h::tl ->
          match unify env ~scope !!!h.lvar !!!h.new_val acc with
          | None -> None
          | Some (p,s) -> helper (is_enlarged || p<>[] ) s tl
      in
      helper false subst prefix

  end

let rec refine : Env.t -> Subst.t -> _ -> Obj.t -> Obj.t = fun env subst do_diseq x ->
  let rec walk' forbidden term =
    let var = Subst.walk env term subst in
    match Env.var env var with
    | None ->
        (match wrap (Obj.repr var) with
          | Unboxed _ -> Obj.repr var
          | Boxed (t, s, f) ->
            let copy = Obj.dup (Obj.repr var) in (* not a shallow copy *)
            let sf =
              if t = Obj.double_array_tag
              then !!! Obj.set_double_field
              else Obj.set_field
            in

            for i = 0 to s-1 do
              sf copy i @@ walk' forbidden (!!!(f i))
            done;
            copy
          | Invalid n -> invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some n when List.mem n forbidden -> var
    | Some n ->
          (* assert (i=n); *)
          let cs : _ list = do_diseq !!!var in
          let cs = List.filter (fun x -> match Env.var env x with Some n -> not (List.mem n forbidden) | None -> true) cs in
          let cs = List.map (walk' ((!!!var : inner_logic).index :: forbidden)) cs in
          Obj.repr {!!!var with constraints = cs}
  in
  walk' [] x

exception Disequality_violated

module type CONSTRAINTS = sig
  type t
  val empty: t
  val show: env:Env.t -> t -> string

  (** [refine env c x] refines [x] and maybe changes constraints [c].
   *  It returns a list of term that [x] should not be equal
   *)
  val refine: Env.t -> Subst.t -> t -> inner_logic -> Obj.t list

  val extend : prefix:Subst.content list -> Env.t -> t -> t

  val check  : prefix:Subst.content list -> Env.t -> Subst.t -> t -> t
end

module FastConstraints : CONSTRAINTS =
struct
  (* A constraint itself is a substitution represented as associative list *)
  type single_constraint = Subst.content list

  module M = struct
    include Map.Make(Int)
    (* and this should be a multimap *)

    let empty : single_constraint list t = empty
    let find_exn : key -> 'a list t -> 'a list = find
    let find key map =
      try find_exn key map
      with Not_found -> []

    let add1 : key -> 'a -> 'a list t -> 'a list t = fun k v m ->
      try let vs = find_exn k m in
          let vs = (*if List.memq v vs then vs else*) v::vs in
          add k vs m
      with Not_found -> add k [v] m

    let replace: key -> 'a list -> 'a list t -> 'a list t = fun k v ->
      if v = [] then remove k
      else add k v
  end

  type t = single_constraint list M.t

  let empty = M.empty
  let bprintf_single ~env b cs =
    let rec helper = function
    | [] -> ()
    | c :: tl ->
          bprintf b "%d -> %s;" (!!!c.Subst.lvar : inner_logic).index (pretty_generic_show (Env.is_var env) c.Subst.new_val);
          helper tl
    in
    helper cs

  let show_single ~env (c: single_constraint) =
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


  let extend : prefix:single_constraint -> Env.t -> t -> t = fun ~prefix env cs ->
    assert (prefix <> []);
    let open Subst in
    let h = List.hd prefix in
    let ans = M.add1 h.lvar.index prefix cs in
    match Env.var env h.new_val with
    | None -> ans
    | Some n ->
        let swapped = { new_val = Obj.repr h.lvar; lvar = (!!!(h.new_val) : inner_logic) } in
        M.add1 n (swapped::(List.tl prefix)) ans

  let split_and_unify ~(prefix:single_constraint) env subst =
    (* There we can save on memory allocations if we will
      do incremental unification of the list *)
    let open Subst in
    let vs,ts = List.split @@
      List.map (fun {lvar;new_val} -> (lvar, new_val)) prefix
    in
    Subst.unify env !!!vs !!!ts non_local_scope subst

  let interacts_with ~prefix (c: single_constraint) : Subst.content option =
    let first_var = (List.hd c).Subst.lvar in
    try Some (List.find (fun cnt -> cnt.Subst.lvar.index = first_var.index) prefix)
    with Not_found ->
      begin
        (* New variable bindings can be not in the substitution. N.B. set-var-val optimization *)
        (* TODO: Maybe pass substitution here and use find function form Subst module *)
        match first_var.subst with
        | Some term -> Some { Subst.lvar=first_var; Subst.new_val=term}
        | None -> None
      end

  type revisiting_result =
    (** This constraint always holds in a substitution *)
    | Obsolete
    (* Constraint has been revisited and should be added for another variable *)
    | Reworked of int * single_constraint
    | Violated

  let check ~prefix env (subst: Subst.t) (c_store: t) : t =
    let revisit_constraint c : revisiting_result =
      let rec helper = function
      | [] -> Violated
      | h::tl ->
          match Subst.(unify env !!!(h.Subst.lvar) h.Subst.new_val) non_local_scope subst with
          | None -> (* non-unifiable, we can forget a constraint *)
              Obsolete
          | Some ([],_) -> helper tl
          | Some ((ph::_) as new_prefix, _)  ->
              Reworked (ph.Subst.lvar.index, new_prefix@tl)
      in
      (* assert (c<>[]); *)
      helper c
    in

    let rec loop2 map = function
    | [] -> map
    | h :: tl ->
        let important = M.find h.Subst.lvar.index map in
        let (acc_cur, acc_other) =
          ListLabels.fold_left important ~init:([],[])
              ~f:(fun (acc_cur,acc_other) cs ->
                    match revisit_constraint cs with
                    | Violated -> raise Disequality_violated
                    | Reworked (idx, new_one) when idx = h.Subst.lvar.index ->
                        (new_one::acc_cur, acc_other)
                    | Reworked (idx, new_one) ->
                        (acc_cur, (idx,new_one)::acc_other)
                    | Obsolete -> (acc_cur, acc_other)
                )
        in
        let map = M.replace h.Subst.lvar.index acc_cur map in
        let map = ListLabels.fold_left ~init:map acc_other ~f:(fun acc (i,cs) -> M.add1 i cs acc) in
        (* We don't need to check newly created constraint with current prefix because it was done
            in a function [revisit_constraint]
          *)
        loop2 map tl
    in
    loop2 c_store prefix

  (* Refine-related stuff goes below *)

  (* [is_subsumed env c xs] checks that [c] is subsumed by some of the constraints in [xs] *)
  let is_subsumed env : single_constraint -> single_constraint list -> _ = fun d d2 ->
    let s = Subst.merge_a_prefix_unsafe ~scope:non_local_scope d Subst.empty in
    let rec helper = function
    | [] -> false
    | h::tl -> begin
        match Subst.merge_a_prefix env ~scope:non_local_scope h s with
        | None -> helper tl
        | Some (_,false) -> true (* TODO: simplify? *)
        | Some (__,_) -> helper tl
      end
    in
    helper d2

  let rem_subsumed env cs =
    let rec helper d acc =
      match d with
      | [] -> acc
      | h::tl when is_subsumed env h tl || is_subsumed env h acc ->
          helper tl acc
      | h:: tl -> helper tl (h::acc)
    in
    helper cs []

  exception ReallyNotEqual
  let simplify_single_constraint ~env ~subst (asked_var: inner_logic) maybe_swap single : single_constraint =
    (* We need this to simplify answer in that case:
     *   subst: [ q -> (a,b); ]
     *   constr: [ (a=/=5) || (b=/=6) ]
     *   extend subst with (a === 3)
     *   ask to refine: q
     *   expected answer: q = (3, b)
     *   without simplification: q = (3, b {{ =/= 6}})
     **)
    let rec helper acc = function
    | [] -> acc
    | cont::tl -> begin
        match Subst.(unify env !!!(cont.lvar) !!!cont.new_val non_local_scope subst) with
        | None -> raise ReallyNotEqual
        | Some ([],_) -> (* terms will be disequal by some other reason. Ignore this part of constraint *)
              if cont.Subst.lvar != asked_var && cont.Subst.new_val != !!!asked_var then []
              else helper acc tl
        | Some (_,_) -> (* this constraint worth printing *)
              helper ((maybe_swap cont) :: acc) tl
      end
    in
    try helper [] single
    with ReallyNotEqual -> []

  let rem_subsumed_opt ~env ~subst asked_var maybe_swap cs_map : single_constraint list =
    (* there we have constraints related to idx. they are the ones stored with key varidx and maybe some others *)
    M.fold (fun _k cs_list (acc: single_constraint list) ->
      (* TODO: we can eliminate List.find by checking _k *)
      ListLabels.fold_left ~init:acc cs_list
        ~f:(fun (acc: single_constraint list) (single: single_constraint) ->
              let single = simplify_single_constraint ~env ~subst asked_var maybe_swap single in
              try let _ = List.find (fun x -> (x.Subst.lvar==asked_var) || (x.Subst.new_val == !!!asked_var)) single in
                    if is_subsumed env single acc
                    then acc
                    else single::acc
              with Not_found -> acc
        )
    )
    cs_map
    []

  let rem_duplicates xs =
    let rec loop acc = function
    | [] -> acc
    | h::tl when List.memq h acc -> loop acc tl
    | h::tl -> loop (h::acc) tl
    in
    loop [] xs

  let refine: Env.t -> Subst.t -> t -> inner_logic -> Obj.t list = fun env subst cs term ->
    (* printfn "going to refine constraints for a variable '%s'" (generic_show term); *)
    let maybe_swap cnt =
      let open Subst in
      (* We always refine logic variables by design, so we can omit checking that term is a variable *)
      (* TODO: I'm not sure that maybe_swap is still needed *)
      if cnt.new_val == !!!term
      then { lvar = !!!term; new_val = Obj.repr cnt.lvar }
      else cnt
    in
    let ans =
      ListLabels.map ~f:(fun prefix ->
        (* For every constraint we need to simplify using current substitution because previously
           we checked only head of the constraint *)
        let cs_sub = Subst.merge_a_prefix_unsafe ~scope:non_local_scope prefix Subst.empty in
        let dest = Subst.walk env !!!term cs_sub in
        assert (term <> dest);
        dest
      ) (rem_subsumed_opt ~env ~subst term maybe_swap cs)
    in
    rem_duplicates ans
end

(*
module DefaultConstraints : CONSTRAINTS =
struct
  type t = Subst.t list

  let empty = []
  let show c = GT.show(GT.list) Subst.show c

  let normalize_store ~prefix env constr =
    (* This implementation ignores first list of prefix which contains variable indicies *)
    let subst  = Subst.of_list prefix in
    let open Subst in
    let prefix = List.split @@ List.map (fun {lvar;new_val} -> (lvar, new_val)) prefix in
    (* There we can save on memory allocations if we will
       do incremental unification of the list *)
    let subsumes subst (vs, ts) =
      try
        match Subst.unify env !!!vs !!!ts non_local_scope (Some subst) with
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

  let extend ~prefix env cs : t = normalize_store ~prefix env cs

  let refine env subs cs term =
    printfn "Constraints.refine";
    list_filter_map cs ~f:(fun cs_sub ->
      let dest = Subst.walk env !!!term cs_sub in
      if dest == term then None else Some dest
    )

  let check ~prefix env subst' cstr =
    (* TODO: only apply constraints with the relevant vars *)
    ListLabels.fold_left cstr ~init:[] ~f:(fun css' cs_sub ->
      (* TODO: here is room for optimization memory usage *)
      let x,t = Subst.split cs_sub in
      try
        let p, s' = Subst.unify env (!!!x) (!!!t) non_local_scope subst' in
        match s' with
        | None -> css'
        | Some _ ->
            match p with
            | [] -> raise Disequality_violated
            | _  -> (Subst.of_list p)::css'
      with Occurs_check -> css'
    )
end*)

(* module Constraints = DefaultConstraints *)
module Constraints = FastConstraints

module State =
  struct
    type t = Env.t * Subst.t * Constraints.t * scope_t
    let empty () = (Env.empty (), Subst.empty, Constraints.empty, new_scope ())
    let env   (env, _, _, _) = env
    let subst (_,s,_,_) = s
    let constraints (_,_,cs,_) = cs

    let show  (env, subst, constr, scp) =
      sprintf "st {%s, %s} scope=%d" (Subst.show subst) (Constraints.show ~env constr) scp
    let new_var (e,_,_,scope) =
      let (x,_) = Env.fresh ~scope e in
      let i = (!!!x : inner_logic).index in
      (x,i)
    let incr_scope (e,subs,cs,scp) = (e,subs,cs, new_scope ())
  end

type 'a goal' = State.t -> 'a
type goal = MKStream.t goal'

let call_fresh f : State.t -> _ = fun (env, subst, constr, scope) ->
  let x, env' = Env.fresh ~scope env in
  f x (env', subst, constr, scope)

let unif_counter = ref 0
let logged_unif_counter = ref 0
let diseq_counter = ref 0
let logged_diseq_counter = ref 0

let report_counters () =
  printfn "total  unifications: %d" !unif_counter;
  printfn "logged unifications: %d" !logged_unif_counter;
  printfn "total diseq calls : %d" !diseq_counter;
  printfn "logged diseq calls : %d" !logged_diseq_counter

let (===) ?loc (x: _ injected) y (env, subst, constr, scope) =
  (* we should always unify two injected types *)
  (* incr unif_counter; *)

  match Subst.unify env x y scope subst with
  | None -> MKStream.nil
  | Some (prefix, s) ->
      try
        let constr' = Constraints.check ~prefix env s constr in
        MKStream.single (env, s, constr', scope)
      with Disequality_violated -> MKStream.nil

let (=/=) x y ((env, subst, constrs, scope) as st) =
  (* For disequalities we unify in non-local scope to prevent defiling *)
  match Subst.unify env x y non_local_scope subst with
  | None -> MKStream.single st
  | Some ([],_) -> MKStream.nil (* this constraint can't be fulfilled *)
  | Some (prefix,_) ->
      let new_constrs = Constraints.extend ~prefix env constrs in
      MKStream.single (env, subst, new_constrs, scope)

let delay : (unit -> goal) -> goal = fun g ->
  fun st -> MKStream.from_fun (fun () -> g () st)

let inc : goal -> goal = fun g st -> MKStream.from_fun (fun () -> g st)

let conj f g st = MKStream.bind (f st) g

let (&&&) = conj

let disj f g st =
  let open MKStream in
  mplus (f st) (MKStream.from_fun (fun () -> g st))

let (|||) = disj

(* mplus_star *)
let rec (?|) = function
| []    -> failwith "wrong argument of ?|"
| [h]   -> h
| h::tl -> h ||| (?| tl)

(* "bind*" *)
let rec (?&) = function
| []   -> failwith "wrong argument of ?&"
| [h]  -> h
| x::y::tl -> ?& ((x &&& y)::tl)

let bind_star = (?&)

let list_fold ~f ~initer xs =
  match xs with
  | [] -> failwith "bad argument"
  | start::xs -> ListLabels.fold_left ~init:(initer start) ~f xs

let list_fold_right0 ~f ~initer xs =
  let rec helper = function
  | [] -> failwith "bad_argument"
  | x::xs -> list_fold ~initer ~f:(fun acc x -> f x acc) (x::xs)
  in
  helper (List.rev xs)

let conde: goal list -> goal = fun xs st ->
  let st = State.incr_scope st in
  list_fold_right0 ~initer:(fun x -> x)
    xs
    ~f:(fun g acc st ->
          MKStream.mplus (g st) @@ MKStream.inc (fun () -> acc st)
      )
  |> (fun g -> MKStream.inc (fun ()  -> g st))

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

let success st = MKStream.single st
let failure _  = MKStream.nil

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

let helper_of_state st : helper =
  !!!(object method isVar x = Env.is_var (State.env st) (Obj.repr x) end)

class type ['a,'b] refined = object
  method is_open: bool
  method prj: 'a
  method refine: (helper -> ('a, 'b) injected -> 'b) -> inj:('a -> 'b) -> 'b
end

let make_rr : ('a, 'b) injected -> State.t -> ('a, 'b) refined = fun x ((env, s, cs, scp) as st) ->
  let ans = !!!(refine env s (Constraints.refine env s cs) (Obj.repr x)) in
  let is_open = has_free_vars (Env.is_var env) (Obj.repr ans) in
  let c: helper = helper_of_state st in

  object(self)
    method is_open = is_open
    method prj = if self#is_open then raise Not_a_value else !!!ans
    method refine refiner ~inj =
      if self#is_open then refiner c ans else inj ans
  end

let prj : ('a, 'b) injected -> 'a = fun x ->
  let rr = make_rr x @@ State.empty () in
  rr#prj

module R : sig
  type ('a, 'b) refiner

  val refiner :  ('a, 'b) injected -> ('a, 'b) refiner

  val apply_refiner : State.t Stream.t -> ('a, 'b) refiner -> ('a, 'b) refined Stream.t
end = struct
  type ('a, 'b) refiner = State.t Stream.t -> ('a, 'b) refined Stream.t

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
      appf (Stream.of_mkstream base) x

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
  let run f = f (State.empty ()) in
  run (adder goalish) |> ApplyLatest.apply app_num |> (currier f)

(* Tracing/debugging stuff *)

let trace msg g = fun state ->
  printf "%s: %s\n%!" msg (State.show state);
  g state

let refine_with_state (env,subs,cs,_) term = refine env subs (Constraints.refine env subs cs) (Obj.repr term)

let project1 ~msg : (helper -> 'b -> string) -> ('a, 'b) injected -> goal = fun shower q st ->
  printf "%s %s\n%!" msg (shower (helper_of_state st) @@ Obj.magic @@ refine_with_state st q);
  success st

let project2 ~msg : (helper -> 'b -> string) -> (('a, 'b) injected as 'v) -> 'v -> goal = fun shower q r st ->
  printf "%s '%s' and '%s'\n%!" msg (shower (helper_of_state st) @@ Obj.magic @@ refine_with_state st q)
                                    (shower (helper_of_state st) @@ Obj.magic @@ refine_with_state st r);
  success st

let project3 ~msg : (helper -> 'b -> string) -> (('a, 'b) injected as 'v) -> 'v -> 'v -> goal = fun shower q r s st ->
  printf "%s '%s' and '%s' and '%s'\n%!" msg
    (shower (helper_of_state st) @@ Obj.magic @@ refine_with_state st q)
    (shower (helper_of_state st) @@ Obj.magic @@ refine_with_state st r)
    (shower (helper_of_state st) @@ Obj.magic @@ refine_with_state st s);
  success st

let unitrace ?loc shower x y = fun st ->
  incr logged_unif_counter;

  let ans = (x === y) st in
  (* printf "%d: unify '%s' and '%s'" !logged_unif_counter (shower (helper_of_state st) x) (shower (helper_of_state st) y);
  (match loc with Some l -> printf " on %s" l | None -> ());

  if MKStream.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans

let diseqtrace shower x y = fun st ->
  incr logged_diseq_counter;
  let ans = (x =/= y) st in
  (* printf "%d: (=/=) '%s' and '%s'" !logged_diseq_counter
    (shower (helper_of_state st) x)
    (shower (helper_of_state st) y);
  if MKStream.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans;;

(* ***************************** a la relational StdLib here ***************  *)

let report_counters () = ()
