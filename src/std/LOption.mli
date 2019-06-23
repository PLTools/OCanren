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
type 'a logic' = 'a logic

(** Synonym for regular option type *)
type 'a t = 'a option

(** Ground option (the regular one) *)
type 'a ground = 'a option

(** Logic option *)
type 'a logic = 'a option logic'

(** GT-compatible typeinfo for [ground] *)
val ground :
  (unit,
   < compare : ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground -> GT.comparison;
     eq      : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
     foldl   : ('a -> 'b -> 'a) -> 'a -> 'b ground -> 'a;
     foldr   : ('a -> 'b -> 'a) -> 'a -> 'b ground -> 'a;
     gmap    : ('a -> 'b) -> 'a ground -> 'b ground;
     html    : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer;
     show    : ('a -> string) -> 'a ground -> string >)
  GT.t

(** GT-compatible typeinfo for [logic] *)
val logic :
  (unit,
   < compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
     eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
     foldl   : ('a -> 'b -> 'a) -> 'a -> 'b logic -> 'a;
     foldr   : ('a -> 'b -> 'a) -> 'a -> 'b logic -> 'a;
     gmap    : ('a -> 'b) -> 'a logic -> 'b logic;
     html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
     show    : ('a -> string) -> 'a logic -> string >)
  GT.t

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