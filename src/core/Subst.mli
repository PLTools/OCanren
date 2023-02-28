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

module Binding :
  sig
    type t =
      { var   : Term.Var.t
      ; term  : Term.t
      }

    val is_relevant : Env.t -> Term.VarSet.t -> t -> bool

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val pp: Format.formatter -> t -> unit
  end

type t

val pp: Format.formatter -> t -> unit

val empty : t

val of_list : Binding.t list -> t
val of_map  : Term.t Term.VarMap.t -> t

val split : t -> Binding.t list

(* [apply env subst x] - applies [subst] to term [x],
 *   i.e. replaces every variable to relevant binding in [subst];
 *)
val apply : Env.t -> t -> 'a -> 'a

(* [is_bound x subst] - checks whether [x] is bound by [subst] *)
val is_bound : Term.Var.t -> t -> bool

(* [freevars env subst x] - returns all free-variables of term [x] *)
val freevars : Env.t -> t -> 'a -> Term.VarSet.t

(* [unify ~subsume ~scope env subst x y] performs unification of two terms [x] and [y] in [subst].
 *   Unification is a process of finding substituion [s] s.t. [s(x) = s(y)].
 *   Returns [None] if two terms are not unifiable.
 *   Otherwise it returns a pair of diff and new substituion.
 *   Diff is a list of pairs (var, term) that were added to the original substituion.
 *
 *   If [subsume] argument is passed and [true] then substituion binds variables only from left term,
 *   (i.e. it returns [s] s.t. [s(x) = y]).
 *   This can be used to perform subsumption check:
 *   [y] is subsumed by [x] (i.e. [x] is more general than [x]) if such a unification succeeds.
 *)
val unify : ?subsume:bool -> ?scope:Term.Var.scope -> Env.t -> t -> 'a -> 'a -> (Binding.t list * t) option

val merge_disjoint : Env.t -> t -> t -> t

(* [merge env s1 s2] merges two substituions *)
val merge : Env.t -> t -> t -> t option

(* [subsumed env s1 s2] checks that [s1] is subsumed by [s2] (i.e. [s2] is more general than [s1]).
 *   Subsumption relation forms a partial order on the set of substitutions.
 *)
val subsumed : Env.t -> t -> t -> bool

module Answer :
  sig
    type t = Term.t

    (* [subsumed env x y] checks that [x] is subsumed by [y] (i.e. [y] is more general than [x]) *)
    val subsumed : Env.t -> t -> t -> bool
  end

val reify : Env.t -> t -> 'a -> Answer.t

IFDEF STATS THEN
(** Walk counter *)
val walk_counter : unit -> int
END
