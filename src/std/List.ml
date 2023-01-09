(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2022
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

open Logic
open Core

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

@type ('a, 'l) list = Nil | Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr, fmt
@type ('a, 'l) t    = ('a, 'l) list with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic' = logic;;

@type 'a ground = ('a, 'a ground) t with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a logic  = ('a, 'a logic) t Logic.logic with show, gmap, html, eq, compare, foldl, foldr, fmt

let ground = {
  ground with
  GT.plugins =
    object(this)
      method html    fa l = GT.html   (list) fa (this#html    fa) l
      method eq      fa l = GT.eq     (list) fa (this#eq      fa) l
      method compare fa l = GT.compare(list) fa (this#compare fa) l
      method foldr   fa l = GT.foldr  (list) fa (this#foldr   fa) l
      method foldl   fa l = GT.foldl  (list) fa (this#foldl   fa) l
      method gmap    fa l = GT.gmap   (list) fa (this#gmap    fa) l
      method fmt     fa fmt (xs : _ ground) =
        Format.fprintf fmt "@[[";
        GT.foldl ground (fun () -> Format.fprintf fmt "%a;@ " fa) () xs;
        Format.fprintf fmt "]@]"
      method show    fa l = "[" ^
        let rec inner l =
          (GT.transform(list)
             (fun fself -> object inherit ['a,'a ground,_]  @list[show] (GT.lift fa) (GT.lift inner) fself
                method! c_Nil   _ _      = ""
                method! c_Cons  i s x xs = (fa x) ^ (match xs with Nil -> "" | _ -> "; " ^ (inner xs) )
              end)
             ()
             l
          )
        in inner l ^ "]"
    end
}

let logic = {
  logic with
  GT.plugins =
    object(this)
      method compare fa l = GT.compare (logic') (GT.compare (list) fa (this#compare fa)) l
      method gmap    fa l = GT.gmap    (logic') (GT.gmap    (list) fa (this#gmap    fa)) l
      method eq      fa l = GT.eq      (logic') (GT.eq      (list) fa (this#eq      fa)) l
      method foldl   fa l = GT.foldl   (logic') (GT.foldl   (list) fa (this#foldl   fa)) l
      method foldr   fa l = GT.foldr   (logic') (GT.foldr   (list) fa (this#foldr   fa)) l
      method html    fa l = GT.html    (logic') (GT.html    (list) fa (this#html    fa)) l
      method fmt fa fmt l = Format.fprintf fmt "%s" (this#show (Format.asprintf "%a" fa) l)
      method show fa l =
        GT.show(logic')
          (fun l -> "[" ^
              let rec inner l =
                GT.transform(t)
                  (fun fself ->
                      object
                         inherit ['a,'a logic, _] @t[show] (GT.lift fa) (GT.lift (GT.show(logic') inner)) fself
                         method! c_Nil   _ _      = ""
                         method! c_Cons  i s x xs =
                           (fa x) ^ (match xs with Value Nil -> "" | _ -> "; " ^ (GT.show(logic') inner xs))
                      end)
                  ()
                  l
               in inner l ^ "]"
          )
          l
    end
}

type 'a groundi = ('a, 'a groundi) t Logic.ilogic
type 'a injected = 'a groundi
let reify : 'a 'b . ('a, 'b) Reifier.t -> ('a groundi, 'b logic) Reifier.t =
  fun ra ->
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
    Reifier.compose Reifier.reify
      ( let* fa = ra in
        let* fr = self in
        let rec foo = function
          | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
          | Value x -> Value (GT.gmap t fa fr x)
        in
        Env.Monad.return foo
      ))

let rec prj_exn : ('a, 'b) Reifier.t -> ('a groundi, 'b ground) Reifier.t =
  fun ra ->
    let open Env.Monad.Syntax in
    Reifier.fix (fun rself ->
      Reifier.compose Reifier.prj_exn
      (let* fa = ra in
      let* fr = rself in
      Env.Monad.return (fun x -> GT.gmap t fa fr x)))

let prj_to_list_exn : ('a, 'b) Reifier.t -> ('a groundi, 'b GT.list) Reifier.t =
  let gmap fa fb = function
    | Nil -> Stdlib.List.([])
    | Cons (h, tl) -> Stdlib.List.cons (fa h) (fb tl)
  in
  let open Env.Monad in
  let fmapt fa fb subj = return gmap <*> fa <*> fb <*> subj in
  fun ra ->
    let open Env.Monad.Syntax in
    Reifier.fix (fun self -> Logic.Reifier.prj_exn <..> chain (fmapt ra self))

let rec prj : (int -> _ ground) -> ('a, 'b) Reifier.t -> ('a groundi, 'b ground) Reifier.t =
  fun onvar ra ->
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun self ->
    Reifier.compose (Reifier.prj (fun _ -> assert false))
    (ra >>= fun fa ->
     self >>= fun fr ->
     Env.Monad.return (fun x -> GT.gmap t fa fr x)))

let nil () : 'a groundi = Logic.inj Nil
let cons : 'a -> 'a groundi -> 'a groundi = fun x y ->
  Logic.inj (Cons (x, y))

let rec of_list f = function
| []    -> Nil
| x::xs -> Cons (f x, of_list f xs)

let rec to_list f = function
| Nil -> []
| Cons (x,xs) -> f x :: to_list f xs

let rec inj f xs = to_logic (GT.gmap list f (inj f) xs)

let rec list = function
| []    -> nil ()
| x::xs -> cons x (list xs);;

let rec logic_to_ground_exn f = function
  | Var (_, _) -> failwith "List.logic_to_ground_exn: variables inside"
  | Value Nil -> Nil
  | Value (Cons (h, tl)) ->
      Cons (f h, logic_to_ground_exn f tl)

let (%) = cons
let (%<) = fun x y -> cons x @@ cons y @@ nil ()
let (!<) = fun x -> cons x @@ nil ()


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
      (p x Bool.truo) &&& (x % a === a');
      (p x Bool.falso) &&& (a === a')
    ]
  in
  foldro folder (nil ()) xs ys

let rec lookupo p xs mx =
  conde [
    (xs === nil ()) &&& (mx === Option.none ());
    Fresh.two (fun h t ->
      (h % t === xs) &&&
      (conde [
        (p h Bool.truo) &&& (mx === (Option.some h));
        (p h Bool.falso) &&& (lookupo p t mx)
      ])
    )
  ]

let rec assoco x xs v =
   Fresh.three (fun a b tl ->
     (xs === (Pair.pair a b) % tl) &&&
     conde [
       (a === x) &&& (b === v);
       (a =/= x) &&& (assoco x tl v)
     ]
   )

let anyo = foldro Bool.oro Bool.falso

let allo = foldro Bool.ando Bool.truo

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

let caro : 'a groundi -> _ -> goal = fun xs h -> call_fresh (fun tl -> xs === (h % tl))
let cdro : 'a Logic.ilogic groundi -> _ -> goal = fun xs tl -> call_fresh (fun h  -> xs === (h % tl))
let hdo = caro
let tlo = cdro
