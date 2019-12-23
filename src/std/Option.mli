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

(** {3 Relational [option]} *)

open Logic
open Core

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Synonym for regular option type *)
@type 'a t = 'a GT.option with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Ground option (the regular one) *)
@type 'a ground = 'a GT.option with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic option *)
@type 'a logic = 'a GT.option logic' with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic injection (for reification) *)
val inj : ('a -> 'b) -> 'a ground -> 'b logic

(** A synonym for injected option *)
type ('a, 'b) groundi = ('a ground, 'b logic) injected

(** Make injected [option] from ground one with injected value *)
val option : ('a, 'b) injected ground -> ('a, 'b) groundi

(** Reifier *)
val reify : (Env.t -> ('a, 'b) injected -> 'b) -> Env.t -> ('a, 'b) groundi -> 'b logic

(** Injection counterpart for constructors *)
val some : ('a, 'b) injected -> ('a, 'b) groundi
val none : unit -> ('a, 'b) groundi
