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

include Logic
include Core

module Stream  = Stream
module Runconf = Runconf
module Timer   = Timer
module VarEnv = VarEnv
module Env = Env

module Std =
  struct

    module Pair    = Pair
    module Option  = Option
    module Bool    = Bool
    module Nat     = Nat
    module List    = List

    let eqo x y t =
      conde [
        (x === y) &&& (t === Bool.truo);
        (x =/= y) &&& (t === Bool.falso);
      ]

    let neqo x y t =
      conde [
        (x =/= y) &&& (t === Bool.truo);
        (x === y) &&& (t === Bool.falso);
      ]

    let nat n = Nat.nat (Nat.of_int n)

    let (%)  = List.cons
    let (%<) = List.(%<)
    let (!<) = List.(!<)
    let nil  = List.nil

    let rec list f = function
    | []    -> nil ()
    | x::xs -> List.cons (f x) (list f xs)

    let rec nat_list = function
    | []    -> nil ()
    | x::xs -> nat x % nat_list xs

    let some = Option.some
    let none = Option.none
    let pair = Pair.pair


    let structural = Core.structural
    let debug_var = Core.debug_var
    let only_head = Core.only_head
  end
