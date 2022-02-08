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

(** {1 Relational [option]} *)

open Logic
open Core

(** {2 GT-related API} *)

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Synonym for regular option type *)
@type 'a t = 'a GT.option with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Ground option (the regular one) *)
@type 'a ground = 'a GT.option with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic option *)
@type 'a logic = 'a GT.option logic' with show, gmap, html, eq, compare, foldl, foldr, fmt

(** {2 Relational API} *)

(** Logic injection (for reification) *)
val inj : ('a -> 'b) -> 'a ground -> 'b logic

(** A synonym for injected option *)
type 'a groundi = 'a ground ilogic

(** Make injected [option] from ground one with injected value *)
val option : 'a ilogic ground -> 'a ilogic groundi

(** Reifier *)
val reify : ('a, 'b) Reifier.t -> ('a groundi, 'b logic) Reifier.t

(* Shallow non-variable projection *)
val prj_exn : ('a, 'b) Reifier.t -> ('a groundi, 'b ground) Reifier.t

(** {3 Constructors} *)
val some : 'a -> 'a groundi

val none : unit -> 'a groundi
