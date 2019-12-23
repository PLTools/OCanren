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

type t

(* [empty] empty disequality constraint store *)
val empty  : t

(* [add env subst diseq x y] adds constraint [x =/= y] into disequality constraint store *)
val add : Env.t -> Subst.t -> t -> 'a -> 'a -> t option

(* [recheck env subst diseq bindings] - checks that disequality is not violated in refined substitution.
 *   [bindings] is a substitution prefix, i.e. new bindings obtained during unification.
 *   This function may rebuild internal representation of constraints and thus it returns new object.
 *   If constraint is violated then [None] is returned.
 *)
val recheck : Env.t -> Subst.t -> t -> Subst.Binding.t list -> t option

(* [project env subst diseq fv] - projects [diseq] into the set of free-variables [fv],
 *   i.e. it extracts only those constraints that are relevant to variables from [fv]
 *)
val project : Env.t -> Subst.t -> t -> Term.VarSet.t -> t

(* [merge_disjoint env subst diseq diseq'] merges two disequality constraints *)
val merge_disjoint : Env.t -> Subst.t -> t -> t -> t

module Answer :
  sig
    (* [Answer.t] result of reification of disequality constraints *)
    type t

    (* [extract a v] returns list of `forbidden` terms for variable [v] *)
    val extract : t -> Term.Var.t -> Term.t list

    val subsumed : Env.t -> t -> t -> bool
  end

val reify : Env.t -> Subst.t -> t -> 'a -> Answer.t list
