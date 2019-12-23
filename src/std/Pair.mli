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

(** {3 Relational pairs} *)

open Logic
open Core

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Synonym for regular option type *)
@type ('a, 'b) t = 'a * 'b with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Ground option (the regular one) *)
@type ('a, 'b) ground = 'a * 'b with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic option *)
@type ('a, 'b) logic = ('a * 'b) logic' with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic injection (for reification) *)
val inj : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) ground -> ('c, 'd) logic

(** A synonym for injected pair *)
type ('a, 'b, 'c, 'd) groundi = (('a, 'c) ground, ('b, 'd) logic) injected

(** Make injected pair from ground one with injected components *)
val pair : ('a, 'b) injected -> ('c, 'd) injected -> ('a, 'b, 'c, 'd) groundi

(** Reifier *)
val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> Env.t -> ('a, 'b, 'c, 'd) groundi -> ('b, 'd) logic

val prjc :
  (Env.t -> ('a, 'b) injected -> 'a) ->
  (Env.t -> ('c, 'd) injected -> 'c) ->
  (int -> ('a,'c) ground GT.list -> ('a, 'c) ground) ->
  Env.t -> ( ('a,'c) ground, ('b, 'd) logic) injected -> ('a, 'c) ground


