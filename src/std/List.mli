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

(** {3 Relational Lists} *)

open Logic
open Core

(** Abstract list type *)
@type ('a, 'l) list =
| Nil
| Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Type synonym to prevent toplevel [logic] from being hidden *)
@type 'a logic' = 'a logic with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Synonym for abstract list type *)
@type ('a, 'l) t = ('a, 'l) list with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Ground lists (isomorphic to regular ones) *)
@type 'a ground = ('a, 'a ground) t with show, gmap, html, eq, compare, foldl, foldr, fmt

(** Logic lists (with the tails as logic lists) *)
@type 'a logic  = ('a, 'a logic) t logic' with show, gmap, html, eq, compare, foldl, foldr, fmt

(** A synonym for injected list *)
type ('a,'b) groundi = ('a ground, 'b logic) injected

(** Constructors *)
val nil : unit -> ('a, 'b) groundi

val cons : ('a, 'b) injected -> ('a, 'b) groundi -> ('a, 'b) groundi

(** [of_list l] converts regular OCaml list [l] into isomorphic OCanren [ground] list *)
val of_list : ('a -> 'b) -> 'a GT.list -> 'b ground

(** [to_list g] converts OCanren list [g] into regular OCaml list *)
val to_list : ('a -> 'b) -> 'a ground -> 'b GT.list

(** [inj x] makes a logic list from a ground one *)
val inj : ('a -> 'b) -> 'a ground -> 'b logic

(** Make injected [list] from ground one of injected elements *)
val list : ('a, 'b) injected GT.list -> ('a, 'b) groundi

(** Reifier *)
val reify : (Env.t -> ('a, 'b) injected -> 'b) -> Env.t -> ('a ground, 'b logic) injected -> 'b logic

val prjc : (Env.t -> ('a, 'b) injected -> 'a) -> (int -> 'a ground GT.list -> 'a ground) ->
  Env.t -> ('a ground, 'b logic) injected -> 'a ground

(** Relational foldr *)
val foldro : (('a, 'b) injected -> ('acc, _ logic' as 'acc2) injected -> ('acc, 'acc2) injected -> goal) -> ('acc, 'acc2) injected -> ('a, 'b) groundi -> ('acc, 'acc2) injected -> goal

(** Relational map *)
val mapo : (('a, 'b) injected -> ('q, 'w) injected -> goal) -> ('a, 'b) groundi -> ('q, 'w) groundi -> goal

(** Relational filter *)
val filtero : (('a, 'b) injected -> Bool.groundi -> goal) -> ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Relational lookup *)
val lookupo : (('a, 'b) injected -> Bool.groundi -> goal) -> ('a, 'b) groundi -> ('a option, 'b option logic') injected -> goal

(** Relational association list lookup *)
val assoco : ('a, 'b logic') injected -> (('a, 'c) Pair.ground, ('b logic', 'd logic') Pair.logic) groundi -> ('c, 'd logic') injected -> goal

(** Boolean list disjunctions *)
val anyo : (Bool.ground, Bool.logic) groundi -> Bool.groundi -> goal

(** Boolean list conjunction *)
val allo : (Bool.ground, Bool.logic) groundi -> Bool.groundi -> goal

(** Relational length *)
val lengtho : (_, _) groundi -> Nat.groundi -> goal

(** Relational append *)
val appendo : ('a, 'b) groundi -> ('a, 'b) groundi  -> ('a, 'b) groundi -> goal

(** Relational reverse *)
val reverso : ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Relational occurrence check (a shortcut) *)
val membero : ('a, 'b logic') groundi  -> ('a, 'b logic') injected  -> goal

(** Relational check for empty list *)
val nullo : _ groundi -> goal

(** Relational head of the list *)
val caro  : ('a, 'b) groundi -> ('a, 'b) injected -> goal

(** Alias for [caro] *)
val hdo   : ('a, 'b) groundi -> ('a, 'b) injected -> goal

(** Relational tail of the list *)
val cdro  : ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Alias for [cdro] *)
val tlo   : ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) injected -> ('a,'b) groundi -> ('a,'b) groundi

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) groundi

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) injected -> ('a, 'b) groundi
