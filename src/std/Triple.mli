(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2023
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(** {1 Relational pairs} *)

open Logic
open Core

(** {2 GT-related API} *)

type ('a, 'b, 'c) t = 'a * 'b * 'c
[@@deriving gt ~options:{ show; gmap; (* html; *) eq; compare; foldl; foldr; fmt }]

type ('a, 'b, 'c) ground          = 'a * 'b * 'c
[@@deriving gt ~options:{ show; gmap; (* html; *) eq; compare; foldl; foldr; fmt }]
type ('a, 'b, 'c) logic           = ('a * 'b * 'c) Logic.logic
[@@deriving gt ~options:{ show; gmap; (* html; *) eq; compare; foldl; foldr; fmt }]

(** {2 Relational API} *)

(** Logic injection (for reification) *)
val inj : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('a, 'c, 'e) ground -> ('b, 'd, 'f) logic

(** A synonym for injected triple *)
type ('a, 'b, 'c) groundi = ('a * 'b * 'c) ilogic

type ('a, 'b, 'c) injected = ('a, 'b, 'c) groundi

(** Make injected triple from ground one with injected components *)
val make : 'a ilogic -> 'b ilogic -> 'c ilogic -> ('a ilogic, 'b ilogic, 'c ilogic) injected

(** An alias for [make] *)
val triple : 'a ilogic -> 'b ilogic -> 'c ilogic -> ('a ilogic, 'b ilogic, 'c ilogic) injected

(** Reifier *)
val reify : ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t -> ('e, 'f) Reifier.t ->
  (('a, 'c, 'e) injected, ('b, 'd, 'f) logic) Reifier.t

val prj_exn :
  ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t -> ('e, 'f) Reifier.t ->
  (('a, 'c, 'e) injected, ('b, 'd, 'f) ground) Reifier.t
