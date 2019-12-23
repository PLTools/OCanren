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

(** {3 Relational booleans} *)

open Logic
open Core

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldr, foldl, gmap, fmt

(** Synonym for boolean type *)
@type t = GT.bool with show, html, eq, compare, foldr, foldl, gmap, fmt

(** Ground boolean (the regular one) *)
@type ground = GT.bool with show, html, eq, compare, foldr, foldl, gmap, fmt

(** Logic boolean *)
@type logic = GT.bool logic' with show, html, eq, compare, foldr, foldl, gmap, fmt

(** Logic injection (for reification) *)
val inj : ground -> logic

(** A synonym for injected boolean; use [(!!)] operator to make a [groundi] from a regular [bool] *)
type groundi = (ground, logic) injected

(** Reifier *)
val reify : Env.t -> groundi -> logic

(** Constants *)
val falso : groundi
val truo  : groundi

(** Sheffer stroke *)
val (|^) : groundi -> groundi -> groundi -> goal

(** Negation *)
val noto : groundi -> groundi -> goal

(** Negation as a goal *)
val (~~) : groundi -> goal

(** Disjunction *)
val oro : groundi -> groundi -> groundi -> goal

(** Disjunction as a goal *)
val (||) : groundi -> groundi -> goal

(** Conjunction *)
val ando : groundi -> groundi -> groundi -> goal

(** Conjunction as a goal *)
val (&&) : groundi -> groundi -> goal
