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

(** {3 Relational numbers} *)

open Logic
open Core

(** Abstract nat type *)
@type 'a nat =
| O
| S of 'a with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Synonym for abstract nat type *)
@type 'a t = 'a nat with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Ground nat are ismorphic for regular one *)
@type ground = ground t with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Logic nat *)
@type logic = logic t logic' with show, html, eq, compare, foldl, foldr, gmap, fmt

(** Logic injection (for reification) *)
val inj : ground -> logic

(** A type synonym for injected nat *)
type groundi = (ground, logic) injected

(** Reifier *)
val reify : Env.t -> groundi -> logic

(* Shallow non-variable projection *)
val prjc : (int -> ground GT.list -> ground) -> Env.t -> groundi -> ground

(** [of_int n] converts integer [n] into [ground]; negative integers become [O] *)
val of_int : int -> ground

(** [to_int g] converts ground [g] into integer *)
val to_int : ground -> int

(** Make injected [nat] from ground one *)
val nat : ground -> groundi

val o : groundi
val s : groundi -> groundi

val zero : groundi
val one  : groundi
val succ : groundi -> groundi

(** Relational addition *)
val addo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [addo] *)
val ( + ) : groundi -> groundi -> groundi -> goal

(** Relational multiplication *)
val mulo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [mulo] *)
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
