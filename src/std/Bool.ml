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

@type 'a logic' = 'a logic                 with show, html, eq, compare, foldr, foldl, gmap, fmt

let logic' = logic;;

@type ground    = GT.bool                  with show, html, eq, compare, foldr, foldl, gmap, fmt
@type t         = GT.bool                  with show, html, eq, compare, foldr, foldl, gmap, fmt
@type logic     = GT.bool logic'           with show, html, eq, compare, foldr, foldl, gmap , fmt

type groundi   = (ground, logic) injected

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
      method show    = GT.show(logic') (GT.show(GT.bool))
    end
}

let inj = to_logic

let reify = Logic.reify

let falso = Logic.inj @@ lift false
let truo  = Logic.inj @@ lift true

let (|^) a b c =
  conde [
    (a === falso) &&& (b === falso) &&& (c === truo );
    (a === falso) &&& (b === truo ) &&& (c === truo );
    (a === truo ) &&& (b === falso) &&& (c === truo );
    (a === truo ) &&& (b === truo ) &&& (c === falso);
  ]

let noto a na = (a |^ a) na

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

let (~~) a   = noto a truo
let (&&) a b = ando a b truo
let (||) a b = oro  a b truo
