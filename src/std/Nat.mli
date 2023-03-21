(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2021
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

(** {1 Relational numbers} *)

open Logic
open Core

(** Abstract nat type *)
@type 'a t =
| O
| S of 'a with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Ground nat are ismorphic for regular one *)
@type ground = ground t with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Logic nat *)
@type logic = logic t Logic.logic with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Type synonyms to comply with the generic naming scheme *)
@type nat       = ground with show, html, eq, compare, foldl, foldr, gmap, fmt
@type nat_logic = logic  with show, html, eq, compare, foldl, foldr, gmap, fmt
                      
(** Logic injection (for reification) *)
val inj : ground -> logic

(** {2 Relational API} *)

(** A type synonym for injected nat *)
type groundi = groundi t Logic.ilogic

type injected = groundi

(** {3:reifiers Reifiers} *)

(** Reifier *)
val reify : (groundi, logic) Reifier.t

(** Shallow non-variable projection *)
val prj_exn : (groundi, ground) Reifier.t

(** Synonyms to comply with the generic naming scheme *)
val reify_nat   : (groundi, logic) Reifier.t
val prj_exn_nat : (groundi, ground) Reifier.t
  
(** [of_int n] converts integer [n] into [ground]; negative integers become [O] *)
val of_int : int -> ground

(** [to_int g] converts ground [g] into integer *)
val to_int : ground -> int

(** Make injected [nat] from ground one *)
val nat : ground -> groundi

(** {3 Constructors} *)

(** A zero. The name {!o} was selected because it looks similar to arabic digit 0. *)
val o : groundi

(** Constructs next number (a successor) after the provided one. *)
val s : groundi -> groundi

(** A synomym for {!o}. *)
val zero : groundi

(** An alias for [s zero]. *)
val one  : groundi

(** A synomym for {!s}. *)
val succ : groundi -> groundi

(** {3 Built-in relations} *)

(** Relational addition. *)
val addo  : groundi -> groundi -> groundi -> goal

(** Infix synonym for [addo]. *)
val ( + ) : groundi -> groundi -> groundi -> goal

(** Relational multiplication. *)
val mulo  : groundi -> groundi -> groundi -> goal

(** Infix synonym for [mulo]. *)
val ( * ) : groundi -> groundi -> groundi -> goal

(** Comparisons *)
val leo : groundi -> groundi -> Bool.groundi -> goal
val geo : groundi -> groundi -> Bool.groundi -> goal
val gto : groundi -> groundi -> Bool.groundi -> goal
val lto : groundi -> groundi -> Bool.groundi -> goal

(** Comparisons as goals *)
val (<=) : groundi -> groundi -> goal
val (>=) : groundi -> groundi -> goal
val (>)  : groundi -> groundi -> goal
val (<)  : groundi -> groundi -> goal

(** Minimum/maximum *)
val maxo : groundi -> groundi -> groundi -> goal
val mino : groundi -> groundi -> groundi -> goal                                   
