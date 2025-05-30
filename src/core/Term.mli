(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2025
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

(* [Term] encapsulates unsafe operations on untyped OCaml's values extended with the logic variables *)

(* [Var] logic variables and operations on them *)
module Var :
  sig
    type env = int

    type scope

    type anchor

    type t =
      { anchor        : anchor;
        env           : env;
        index         : int;
        mutable subst : Obj.t option;
        scope         : scope;
        constraints   : Obj.t list
      }

    val tabling_env : env

    val non_local_scope : scope

    val new_scope : unit -> scope

    val valid_anchor : anchor -> bool

    val dummy : t

    val make : env:env -> scope:scope -> int -> t

    val reify : ('a -> 'b) -> t -> int * 'b list

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val hash : t -> int

    val is_wildcard : t -> bool

    val make_wc : env:env -> scope:scope -> t
  end

module VarSet : Set.S with type elt = Var.t

module VarTbl : Hashtbl.S with type key = Var.t

module VarMap :
  sig
    include Map.S with type key = Var.t

    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

    val iteri: (int -> key -> 'a -> unit) -> 'a t -> unit

  end

(* [t] type of untyped OCaml term *)
type t = Obj.t

type value

val repr : 'a -> t

(* [var x] if [x] is logic variable returns it, otherwise returns [None] *)
val var : 'a -> Var.t option

(* [map ~fvar ~fval x] map over OCaml's value extended with logic variables;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
 *)
val map : fvar:(Var.t -> t) -> fval:(value -> t) -> t -> t

(* [iter ~fvar ~fval x] iteration over OCaml's value extended with logic variables;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
 *)
val iter : fvar:(Var.t -> unit) -> fval:(value -> unit) -> t -> unit

(* [fold ~fvar ~fval ~init x] fold over OCaml's value extended with logic variables;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
 *)
val fold : fvar:('a -> Var.t -> 'a) -> fval:('a -> value -> 'a) -> init:'a -> t -> 'a

exception Different_shape of int * int

type label = L | R

(* [fold ~fvar ~fval ~fvarval ~init x y] folds two OCaml's value extended with logic variables simultaneously;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
 *   if it finds logic variable in one term but regular value in another term in same place, it calls [fk];
 *   if two terms cannot be traversed simultaneously raises exception [Different_shape (tx, ty)],
 *   where [tx] and [ty] are Ocaml tags of disparate values
 *)
val fold2 :
  fvar:('a -> Var.t -> Var.t -> 'a) ->
  fval:('a -> value -> value -> 'a)  ->
  fk:('a -> label -> Var.t -> t -> 'a) ->
  init:'a -> t -> t -> 'a

val equal   : t -> t -> bool
val compare : t -> t -> int
val hash    : t -> int

val show : t -> string
val describe_var : Format.formatter -> Var.t -> unit
val pp : Format.formatter -> 'a -> unit
