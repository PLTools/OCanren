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

(** {1 Relational Lists} *)

open Logic
open Core

(** Abstract list type *)
@type ('a, 'l) list =
| Nil
| Cons of 'a * 'l
with show, gmap, html, eq, compare, foldl, foldr, fmt

(** {2 GT-related API} *)

(** Synonym for abstract list type *)
@type ('a, 'l) t = ('a, 'l) list with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Ground lists (isomorphic to regular ones) *)
@type 'a ground = ('a, 'a ground) t with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic lists (with the tails as logic lists) *)
@type 'a logic  = ('a, 'a logic) t Logic.logic with show, gmap, html, eq, compare, foldl, foldr, fmt

(** {2 Relational API} *)

(** A synonym for injected list *)
type 'a groundi = ('a, 'a groundi) t Logic.ilogic

type 'a injected = 'a groundi

(** {3 Conversions between data types} *)

(** The [of_list l] converts regular OCaml list [l] into isomorphic OCanren [ground] list.
    See [to_list] for a reverse conversion. *)
val of_list : ('a -> 'b) -> 'a GT.list -> 'b ground

(** The [to_list g] converts OCanren list [g] into regular OCaml list. See [of_list]
    for a reverse conversion. *)
val to_list : ('a -> 'b) -> 'a ground -> 'b GT.list

(** The [inj x] makes a logic list from a ground one. See [logic_to_ground_exn]
    for a partial reverse conversion. *)
val inj : ('a -> 'b) -> 'a ground -> 'b logic

(** Converts a logic list to ground one.
    @raise [Failure] when a logic variable occurs inside a list. *)
val logic_to_ground_exn: ('a -> 'b) -> 'a logic -> 'b ground

(** Make injected [list] from ground one of injected elements. The reverse conversion
    is availble only through reifiers (see {!section-reifiers} for details). *)
val list : 'a GT.list -> 'a groundi

(** {3 Constructors} *)

(** A logical empty list. Extra unit parameter prevents weak type variables. *)
val nil : unit -> 'a groundi

(** A dual for [cons] (a.k.a. [::]) constructor. *)
val cons : 'a  -> 'a groundi -> 'a groundi

(** Infix synonym for {!cons} *)
val (%) : 'a  -> 'a groundi -> 'a groundi

(** [x %< y] is a synonym for [cons x (cons y (nil ()))] *)
val (%<) : 'a  -> 'a -> 'a groundi

(** [!< x] is a synonym for [cons x (nil ())] *)
val (!<) : 'a  ->  'a groundi

(** {3:reifiers Reifiers} *)

(** Reifier *)
val reify :  ('a, 'b) Reifier.t -> ('a groundi, 'b logic) Reifier.t

val prj_exn : ('a, 'b) Reifier.t -> ('a groundi, 'b ground) Reifier.t

val prj_to_list_exn :  ('a, 'b) Reifier.t -> ('a groundi, 'b GT.list) Reifier.t

val prj : (int -> 'b ground) -> ('a, 'b) Reifier.t -> ('a groundi, 'b ground) Reifier.t

(** {3 Built-in relations} *)

(** Relational foldr *)
val foldro :
  ('x ilogic as 'a -> 'acc ilogic -> 'acc ilogic -> goal) ->
  'acc ilogic ->
  'a groundi ->
  'acc ilogic -> goal

(** Relational map *)
val mapo : ('x ilogic as 'a -> ('y ilogic as 'b) -> goal) -> 'a groundi -> 'b groundi -> goal

(** Relational filter *)
val filtero : ('x ilogic as 'a -> Bool.groundi -> goal) -> 'a groundi -> 'a groundi -> goal

(** Relational lookup *)
val lookupo : ('x ilogic as 'a -> Bool.groundi -> goal) -> 'a groundi -> 'a Option.groundi -> goal

(** Relational association list lookup *)
val assoco : 'a ilogic -> ('a ilogic, 'c ilogic ) Pair.groundi groundi -> 'c ilogic -> goal

(** Boolean list disjunctions *)
val anyo : Bool.groundi groundi -> Bool.groundi -> goal

(** Boolean list conjunction *)
val allo : Bool.groundi groundi -> Bool.groundi -> goal


(** Relational length *)
val lengtho : 'a ilogic groundi -> Nat.groundi -> goal

(** Relational append *)
val appendo : (_ ilogic as 'a) groundi -> 'a groundi -> 'a groundi -> goal

(** Relational reverse *)
val reverso : (_ ilogic as 'a)groundi -> 'a groundi -> goal

(** Relational occurrence check (a shortcut) *)
val membero : 'a ilogic groundi  -> 'a ilogic  -> goal

(** Relational check for empty list *)
val nullo : _ groundi -> goal

(** Relational head of the list *)
val caro  : 'a groundi -> 'a -> goal

(** Alias for [caro] *)
val hdo   : 'a groundi -> 'a -> goal

(** Relational tail of the list *)
val cdro  : 'a Logic.ilogic groundi -> 'a Logic.ilogic groundi -> goal

(** Alias for [cdro] *)
val tlo   : 'a Logic.ilogic groundi -> 'a Logic.ilogic groundi -> goal
