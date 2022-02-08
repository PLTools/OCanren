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

(** {3 Logic values} *)

(** A type of a logic value *)
@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr, fmt

(** [to_logic x] makes a logic value from a regular one *)
val to_logic : 'a -> 'a logic

(** [from_logic x] makes a regular value from a logic one.
    Raises exception [Not_a_value] if [x] contains free variables
*)
val from_logic : 'a logic -> 'a

(** {3 Injections/projections} *)

(**  The type [('a, 'b) injected] describes an injection of a type ['a] into ['b] *)
type 'a ilogic

module Reifier : sig
  (* Reifier from type `'a` into type `'b` is an `'a -> 'b` function
    * dipped into the `Env.t` monad, will see how it plays later.
    * Perhaps, it is possible to not expose the reifier type and make it itself
    * a monad or something else that composes nicely, but I haven't figured out yet.
    *)
  type ('a, 'b) t = ('a -> 'b) Env.Monad.t

  (* Some predefined reifiers from which other reifiers will be composed *)

  (* this one transforms implicit logic value into regular logic value *)
  val reify : ('a ilogic, 'a logic) t

  (* this one projects implicit logic into the underlying type,
    * handling variables with the help of the user provided function
    *)
  (* val prj : (int -> 'a) -> ('a ilogic, 'a) t *)

  (* this one projects implicit logic into the underlying type,
    * raising an exception if it finds a variable
    *)
  val prj_exn : ('a ilogic, 'a) t

  val prj : (int -> 'a) -> ('a ilogic, 'a) t


  (* Interesting part --- we can apply a reifier to a value dipped into `State.t` comonad *)
  (* val apply : ('a, 'b) t -> 'a State.t -> 'b *)

  (* composition of two reifiers *)
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  (* Reifier is a profunctor, so we get combinators
   * to compose reifiers with regular functions
   *)
  val fmap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  val fcomap : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t

  val fix: ( ('a, 'b) t as 'c -> 'c) -> 'c

  val rework : fv:('a Env.m -> 'b Env.m)
      -> ('a logic Env.m -> 'b logic Env.m)
      -> 'a logic Env.m
      -> 'b logic Env.m

  (** Fix point combinator for Call-by-value *)
  val zed: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
end

(** [inj x] injects [x] into logical [x] *)
val inj : 'a -> 'a ilogic

(** A synonim to [inj]. To be removed *)
val inji : 'a -> 'a ilogic

(** A synonym for [fun x -> inj @@ lift x] (for non-parametric types) *)
val (!!) : 'a -> 'a ilogic

(** [prj_exn x] returns a regular value from injected representation.
    Raises exception [Not_a_value] if [x] contains free variables
 *)
val prj_exn : ('a ilogic, 'a) Reifier.t

val prj : (int -> 'a) -> ('a ilogic, 'a) Reifier.t

(** A default shallow reifier. An alias of [Reifier.reify] *)
val reify : ('a ilogic, 'a logic) Reifier.t

(** The exception is raised when we try to extract a regular term from the answer with some free variables *)
exception Not_a_value

(** Reification result *)
class type ['a] reified =
object
  (** Returns [true] if the term has any free logic variable inside *)
  method is_open: bool

  (** Gets the answer as a logic value using provided injection function [inj] *)
  method reify: 'b . ('a ilogic, 'b) Reifier.t -> 'b
end

val make_rr : Env.t -> 'a ilogic -> 'a reified
