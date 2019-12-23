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

@type 'a nat = O | S of 'a with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a logic' = 'a logic with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic' = logic

module X =
  struct
    @type 'a t = 'a nat with show, gmap, html, eq, compare, foldl, foldr, fmt
    let fmap f x = GT.gmap (t) f x
  end

include X

module F = Fmap (X)

@type ground  = ground t                 with show, gmap, html, eq, compare, foldl, foldr, fmt
@type logic   = logic t logic'           with show, gmap, html, eq, compare, foldl, foldr, fmt

type groundi = (ground, logic) injected

let logic = {
  logic with
  GT.plugins =
    object(this)
      method compare = logic.GT.plugins#compare
      method gmap    = logic.GT.plugins#gmap
      method eq      = logic.GT.plugins#eq
      method foldl   = logic.GT.plugins#foldl
      method foldr   = logic.GT.plugins#foldr
      method html    = logic.GT.plugins#html
      method fmt     = logic.GT.plugins#fmt
      method show    = GT.show(logic') (fun l -> GT.show(t) this#show l)
    end
}

let rec of_int n = if n <= 0 then O else S (of_int (n-1))
let rec to_int   = function O -> 0 | S n -> 1 + to_int n

let rec inj n = to_logic (GT.(gmap nat) inj n)

let rec reify h n = F.reify reify h n
let rec prjc onvar env n = F.prjc (prjc onvar) onvar env n

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
    (x === o) &&& (b === Bool.truo);
    (x =/= o) &&& (y === o) &&& (b === Bool.falso);
    Fresh.two (fun x' y' ->
      (x === (s x')) &&& (y === (s y')) &&& (leo x' y' b)
    )
  ]

let geo x y b = leo y x b

let (<=) x y = leo x y Bool.truo
let (>=) x y = geo x y Bool.truo

let rec gto x y b = conde
  [ (x =/= o) &&& (y === o) &&& (b === Bool.truo)
  ; (x === o) &&& (b === Bool.falso)
  ; Fresh.two (fun x' y' ->
      (x === s x') &&& (y === s y') &&& (gto x' y' b)
    )
  ]

let lto x y b = gto y x b

let (>) x y = gto x y Bool.truo
let (<) x y = lto x y Bool.truo
