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

@type ground     = GT.bool              with show, html, eq, compare, foldr, foldl, gmap, fmt
@type t          = GT.bool              with show, html, eq, compare, foldr, foldl, gmap, fmt
@type logic      = GT.bool Logic.logic  with show, html, eq, compare, foldr, foldl, gmap, fmt
                                                                                                                                     
@type bool       = ground               with show, html, eq, compare, foldr, foldl, gmap, fmt
@type bool_logic = logic                with show, html, eq, compare, foldr, foldl, gmap, fmt

type groundi   = ground ilogic

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
      method show    = GT.show(Logic.logic) (GT.show(GT.bool))
    end
}

let inj = to_logic

let reify : (bool ilogic, bool Logic.logic) Reifier.t = Logic.reify
let prj_exn : (bool ilogic, bool) Reifier.t = Logic.prj_exn

let reify_bool = reify
let prj_exn_bool = prj_exn
                 
let falso = Logic.inj false
let truo  = Logic.inj true

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
