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

type t

val empty         : unit -> t

val create        : anchor:Term.Var.env -> t

val fresh         : scope:Term.Var.scope -> t -> 'a

val check         : t -> Term.Var.t -> bool

val check_exn     : t -> Term.Var.t -> unit

val is_var        : t -> 'a -> bool

val var           : t -> 'a -> Term.Var.t option

val freevars      : t -> 'a -> Term.VarSet.t

val is_open       : t -> 'a -> bool

val equal         : t -> t -> bool

module Monad : sig
  (* `'a Env.t` --- essentially a reader monad *)
  type nonrec 'a t = t -> 'a

  val return : 'a -> 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val (<*>):  ('a -> 'b) t -> 'a t -> 'b t

  val chain :  ('a t -> 'b t) -> ('a -> 'b) t

  val (<..>): ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

  val list_mapm : f:('a t -> 'b t) -> 'a list -> 'b list t

  (* Do-notation is avaliable since OCaml 4.08 *)
  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t ) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

type 'a m = 'a Monad.t
