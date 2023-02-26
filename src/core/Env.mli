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

(**
  A distinct environment is associated with each {!OCanren.run}. Every fresh variable is
  associated with an emvironment. Using nested run's with reusing of fresh variables
  from the previous run will lead to runtime error due to usage of alien environment. *)

type t

(** Creates a new environment. Environment has an internal state: a counter of
    the variable index introduced last. That's why {!empty} has extra unit argument. *)
val empty         : unit -> t

val create        : anchor:Term.Var.env -> t

(** Creating a fresh variable takes an extra argument [scope] to decide if
    storing a substituted value inside a variable is OKay.

    The idea was recommended by
    {{: https://github.com/michaelballantyne/faster-minikanren#set-var-val }faster-miniKanren}
    implementation. *)
val fresh         : scope:Term.Var.scope -> t -> 'a

val wc : scope:Term.Var.scope -> t -> 'a

val check         : t -> Term.Var.t -> bool

val check_exn     : t -> Term.Var.t -> unit

val is_var        : t -> 'a -> bool

val is_wc        : t -> 'a -> bool

val var           : t -> 'a -> Term.Var.t option

val freevars      : t -> 'a -> Term.VarSet.t

val is_open       : t -> 'a -> bool

val equal         : t -> t -> bool

(** Essentially, a reader monad over Env.t. Useful for reification. *)
module Monad : sig

  (** @canonical OCanren.Env.m *)
  type nonrec 'a t = t -> 'a

  val return : 'a -> 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val (<*>):  ('a -> 'b) t -> 'a t -> 'b t

  (** Three functions {!chain}, {!(<..>)} and {!list_mapm} are used for constructing a reifier. *)

  val chain :  ('a t -> 'b t) -> ('a -> 'b) t

  val (<..>): ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

  val list_mapm : f:('a t -> 'b t) -> 'a list -> 'b list t

  (** Do-notation is avaliable since OCaml 4.08. See OCaml manual for details. *)

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t ) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

(** An alias for a reader monad {!Monad.t}. *)
type 'a m = 'a Monad.t
