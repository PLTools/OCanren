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
type 'a logic' = 'a logic

(** Synonym for boolean type *)
type t = bool

(** Ground boolean (the regular one) *)
type ground = bool

(** Logic boolean *)
type logic = bool logic'

(** GT-compatible typeinfo for [ground] *)
val ground :
  (unit,
   < compare : ground -> ground -> GT.comparison;
     eq      : ground -> ground -> bool;
     foldl   : 'a -> ground -> 'a;
     foldr   : 'a -> ground -> 'a;
     gmap    : ground -> ground;
     html    : ground -> HTML.viewer;
     show    : ground -> string >, unit)
  GT.t

(** GT-compatible typeinfo for [logic] *)
val logic :
  (unit,
   < compare : logic -> logic -> GT.comparison;
     eq      : logic -> logic -> bool;
     foldl   : 'a -> logic -> 'a;
     foldr   : 'a -> logic -> 'a;
     gmap    : logic -> logic;
     html    : logic -> HTML.viewer;
     show    : logic -> string >, unit)
  GT.t

(** Logic injection (for reification) *)
val inj : ground -> logic

(** A synonym for injected boolean; use [(!!)] operator to make a [groundi] from a regular [bool] *)
type groundi = (ground, logic) injected

(** Reifier *)
val reify : VarEnv.t -> groundi -> logic

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
