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

(** {1 Relational pairs} *)

open Logic
open Core

(** {2 GT-related API} *)

@type ('a, 'b) t = 'a * 'b with show, gmap, html, eq, compare, foldl, foldr, fmt

@type ('a, 'b) ground = 'a * 'b with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic option *)
@type ('a, 'b) logic = ('a * 'b) Logic.logic with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Type synonyms to comply with the generic naming scheme *)
@type ('a, 'b) pair       = ('a, 'b) ground with show, gmap, html, eq, compare, foldl, foldr, fmt
@type ('a, 'b) pair_logic = ('a, 'b) logic  with show, gmap, html, eq, compare, foldl, foldr, fmt
                                                                                                                                          
(** {2 Relational API} *)

(** Logic injection (for reification) *)
val inj : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) ground -> ('c, 'd) logic

(** A synonym for injected pair *)
type ('a, 'b) groundi = ('a * 'b) ilogic

type ('a, 'b) injected = ('a, 'b) groundi

(** Make injected pair from ground one with injected components *)
val pair : 'a ilogic -> 'b ilogic -> ('a ilogic, 'b ilogic) groundi

(** {3:reifiers Reifiers} *)
val reify : ('a,'b) Reifier.t -> ('c,'d) Reifier.t ->
  ( ('a, 'c) groundi, ('b, 'd) logic ) Reifier.t

val prj_exn :
  ('a, 'b) Reifier.t -> ('c,'d) Reifier.t ->
  ( ('a, 'c) groundi, ('b, 'd) ground) Reifier.t

(** Synonyms to comply with the generic naming scheme *)
val reify_pair : ('a,'b) Reifier.t -> ('c,'d) Reifier.t ->
  ( ('a, 'c) groundi, ('b, 'd) logic ) Reifier.t

val prj_exn_pair :
  ('a, 'b) Reifier.t -> ('c,'d) Reifier.t ->
  ( ('a, 'c) groundi, ('b, 'd) ground) Reifier.t
    
