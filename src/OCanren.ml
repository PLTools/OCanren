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

include Logic
include Core

module Stream  = Stream
module Runconf = Runconf

(** See also {!Install_timer} *)
module Timer   = Timer
module Env = Env

(** A standard library for logic programs. *)
module Std =
  struct
    (** Logic pairs *)
    module Pair    = Pair
    module Triple = Triple
    (** Logic optional values *)
    module Option  = Option
    module Bool    = Bool

    (** Logic Peano natural numbers *)
    module Nat     = Nat

    (** Logic lists *)
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

    (** An alias for {!OCanren.Std.List.cons}. *)
    let (%)  = List.cons

    (** An alias for {!OCanren.Std.List.(%<)}. *)
    let (%<) = List.(%<)

    (** An alias for {!OCanren.Std.List.(!<)}. *)
    let (!<) = List.(!<)

    (** An alias for {!OCanren.Std.List.nil}. *)
    let nil  = List.nil

    let rec list f = function
    | []    -> nil ()
    | x::xs -> List.cons (f x) (list f xs)

    let rec nat_list = function
    | []    -> nil ()
    | x::xs -> nat x % nat_list xs

    (** An alias for {!OCanren.Std.Option.some}. *)
    let some = Option.some

    (** An alias for {!OCanren.Std.Option.none}. *)
    let none = Option.none

    (** An alias for {!OCanren.Std.Pair.pair}. *)
    let pair = Pair.pair
    let triple = Triple.triple

    let structural = Core.structural
    let debug_var = Core.debug_var
    let only_head = Core.only_head


  end
IFDEF STATS THEN
module Peep = Peep

let _ = Peep.unification_counter
END
