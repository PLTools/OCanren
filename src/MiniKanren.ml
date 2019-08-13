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

include RStream
include Logic
include Core

module Std =
  struct

    module LPair    = LPair
    module LOption  = LOption
    module LBool    = LBool
    module LNat     = LNat
    module LList    = LList

    let eqo x y t =
      conde [
        (x === y) &&& (t === LBool.truo);
        (x =/= y) &&& (t === LBool.falso);
      ]

    let neqo x y t =
      conde [
        (x =/= y) &&& (t === LBool.truo);
        (x === y) &&& (t === LBool.falso);
      ]

    let nat n = LNat.nat (LNat.of_int n)

    let (%)  = LList.cons
    let (%<) = LList.(%<)
    let (!<) = LList.(!<)
    let nil  = LList.nil

    let rec list f = function
    | []    -> nil ()
    | x::xs -> LList.cons (f x) (list f xs)

    let rec nat_list = function
    | []    -> nil ()
    | x::xs -> nat x % nat_list xs

    let some = LOption.some
    let none = LOption.none
    let pair = LPair.pair
  end
