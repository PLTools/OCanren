(*
 * OCanren.
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

open Logic
open Core

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

@type ('a, 'l) list = Nil | Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a logic'     = 'a logic              with show, gmap, html, eq, compare, foldl, foldr, fmt
@type ('a, 'l) t    = ('a, 'l) list         with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic' = logic

module X =
  struct
    @type ('a,'b) t = ('a, 'b) list with show, gmap, html, eq, compare, foldl, foldr, fmt
    let fmap f g x = GT.gmap list f g x
  end

module F = Fmap2 (X)

let nil ()   = Logic.inj @@ F.distrib Nil
let cons x y = Logic.inj @@ F.distrib (Cons (x, y));;

@type 'a ground = ('a, 'a ground) t with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a logic  = ('a, 'a logic) t logic' with show, gmap, html, eq, compare, foldl, foldr, fmt

let rec reify r1 h = F.reify r1 (reify r1) h

let rec prjc fa onvar env xs = F.prjc fa (prjc fa onvar) onvar env xs

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
                method c_Nil   _ _      = ""
                method c_Cons  i s x xs = (fa x) ^ (match xs with Nil -> "" | _ -> "; " ^ (inner xs) )
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
                         method c_Nil   _ _      = ""
                         method c_Cons  i s x xs =
                           (fa x) ^ (match xs with Value Nil -> "" | _ -> "; " ^ (GT.show(logic') inner xs))
                      end)
                  ()
                  l
               in inner l ^ "]"
          )
          l
    end
}

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

type ('a,'b) groundi = ('a ground, 'b logic) injected

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

let rec membero (l: (_,_) groundi) a =
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
