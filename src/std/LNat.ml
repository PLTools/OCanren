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

@type 'a nat = O | S of 'a with show, gmap, html, eq, compare, foldl, foldr;;

type 'a logic' = 'a logic
let logic' = logic

module X =
  struct
    type 'a t = 'a nat
    let fmap f x = GT.(gmap nat) f x
  end

include X

module F = Fmap (X)

type ground = ground t
type logic = logic t logic'
type groundi = (ground, logic) injected

let ground = {
  GT.gcata = ();
  GT.fix = ();
  GT.plugins =
    object(this)
      method html    n = GT.html   (nat) this#html    n
      method eq      n = GT.eq     (nat) this#eq      n
      method compare n = GT.compare(nat) this#compare n
      method foldr   n = GT.foldr  (nat) this#foldr   n
      method foldl   n = GT.foldl  (nat) this#foldl   n
      method gmap    n = GT.gmap   (nat) this#gmap    n
      method show    n = GT.show   (nat) this#show    n
    end
}

let logic = {
  GT.gcata = ();
  GT.fix = ();
  GT.plugins =
    object(this)
      method html    n   = GT.html   (logic') (GT.html   (nat) this#html   ) n
      method eq      n m = GT.eq     (logic') (GT.eq     (nat) this#eq     ) n m
      method compare n m = GT.compare(logic') (GT.compare(nat) this#compare) n m
      method foldr   a n = GT.foldr  (logic') (GT.foldr  (nat) this#foldr  ) a n
      method foldl   a n = GT.foldl  (logic') (GT.foldl  (nat) this#foldl  ) a n
      method gmap    n   = GT.gmap   (logic') (GT.gmap   (nat) this#gmap   ) n
      method show    n   = GT.show   (logic') (GT.show   (nat) this#show   ) n
    end
}

let rec of_int n = if n <= 0 then O else S (of_int (n-1))
let rec to_int   = function O -> 0 | S n -> 1 + to_int n

let rec inj n = to_logic (GT.(gmap nat) inj n)

let rec reify h n = F.reify reify h n

let o   = Logic.inj @@ F.distrib O
let s x = Logic.inj @@ F.distrib (S x)

let rec nat n = Logic.inj @@ F.distrib @@ X.fmap nat n

let zero = o
let one  = s o
let succ = s

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
  conde
    [ (x === o) &&& (z === o)
    ; Fresh.two (fun x' z' ->
        (x === (s x')) &&&
        (addo y z' z) &&&
        (mulo x' y z')
      )
    ]

let ( * ) = mulo

let rec leo x y b =
  conde [
    (x === o) &&& (b === LBool.truo);
    (x =/= o) &&& (y === o) &&& (b === LBool.falso);
    Fresh.two (fun x' y' ->
      (x === (s x')) &&& (y === (s y')) &&& (leo x' y' b)
    )
  ]

let geo x y b = leo y x b

let (<=) x y = leo x y LBool.truo
let (>=) x y = geo x y LBool.truo

let rec gto x y b = conde
  [ (x =/= o) &&& (y === o) &&& (b === LBool.truo)
  ; (x === o) &&& (b === LBool.falso)
  ; Fresh.two (fun x' y' ->
      (x === s x') &&& (y === s y') &&& (gto x' y' b)
    )
  ]

let lto x y b = gto y x b

let (>) x y = gto x y LBool.truo
let (<) x y = lto x y LBool.truo
