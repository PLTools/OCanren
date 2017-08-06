(*
 * MiniKanrenStd: miniKanren standard library.
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


open MiniKanrenCore

module Reify : 
  sig
  val bool   : helper -> (bool, bool logic) injected -> bool logic
  val int    : helper -> (int, int logic) injected -> int logic
  val string : helper -> (string, string logic) injected -> string logic
  val pair   : (helper -> ('a,'b) injected -> 'b) ->
               (helper -> ('c,'d) injected -> 'd) ->
               helper -> ('a * 'c, ('b * 'd) logic as 'r) injected -> 'r
  val triple : (helper -> ('a,'b) injected -> 'b) ->
               (helper -> ('c,'d) injected -> 'd) ->
               (helper -> ('e,'f) injected -> 'f) ->
               helper -> ('a * 'c * 'e, ('b * 'd * 'f) logic as 'r) injected -> 'r
end;;

(** {2 Standard relational library } *)

(** {3 Predefined types (lists, nats, bools etc.)} *)

(** Abstract list type *)
@type ('a, 'l) llist =
| Nil
| Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr

(** Abstract nat type *)
@type 'a lnat =
| O
| S of 'a with show, html, eq, compare, foldl, foldr, gmap

module Option : sig
  type 'a t = 'a option
  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val reify :
    (helper -> ('a, 'b) injected -> 'b) ->
    helper -> ('a t, 'b t logic) injected -> 'b t logic

  val some : ('a, 'b) injected -> ('a t, 'b t logic) injected
  val none : unit -> ('a t, 'b t logic) injected
end

(** {3 Relations on booleans} *)
module Bool :
  sig
    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

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
         show    : ground -> string >)
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
         show    : logic -> string >)
      GT.t

    (** A synonym for injected boolean *)
    type groundi = (ground, logic) injected

    (** Constants *)
    val false_ : groundi
    val true_  : groundi

    (** Sheffer stroke *)
    val (|^) : groundi -> groundi -> groundi -> goal

    (** Negation *)
    val noto' : groundi -> groundi -> goal

    (** Negation as a goal *)
    val noto : groundi -> goal

    (** Disjunction *)
    val oro : groundi -> groundi -> groundi -> goal

    (** Disjunction as a goal *)
    val (||) : groundi -> groundi -> goal

    (** Conjunction *)
    val ando : groundi -> groundi -> groundi -> goal

    (** Conjunction as a goal *)
    val (&&) : groundi -> groundi -> goal
  end

(** Equality as boolean relation *)
val eqo : ('a, 'b logic) injected -> ('a, 'b logic) injected -> Bool.groundi -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b logic) injected -> ('a, 'b logic) injected -> Bool.groundi -> goal

(** {3 Relations on nats} *)
module Nat :
  sig
    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for abstract nat type *)
    type 'a t = 'a lnat

    (** Ground nat are ismorphic for regular one *)
    type ground = ground t

    (** Logic nat *)
    type logic = logic t logic'

    (** Reifier *)
    val reify : helper -> (ground, logic) injected -> logic

    (** GT-compatible typeinfo for [ground] *)
    val ground :
      (unit,
       < compare : ground -> ground -> GT.comparison;
         eq      : ground -> ground -> bool;
         foldl   : 'a -> ground -> 'a;
         foldr   : 'a -> ground -> 'a;
         gmap    : ground -> ground;
         html    : ground -> HTML.viewer;
         show    : ground -> string >)
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
         show    : logic -> string >)
      GT.t

    (** A type synonym for injected nat *)
    type groundi = (ground, logic) injected

    (** [of_int n] converts integer [n] into [ground]; negative integers become [O] *)
    val of_int : int -> ground

    (** [to_int g] converts ground [g] into integer *)
    val to_int : ground -> int

    (** [to_logic x] makes logic nat from ground one *)
    val to_logic : ground -> logic

    (** [from_logic x] makes ground nat from logic one.
        Raises exception [Not_a_value] if [x] contains logic variables.*)
    val from_logic : logic -> ground

    (** Make injected nat from ground one *)
    val inj : ground -> groundi

    val zero : groundi
    val one  : groundi
    val succ : groundi -> groundi

    (** Relational addition *)
    val addo  : groundi -> groundi -> groundi -> goal

    (** Infix syninym for [addo] *)
    val ( + ) : groundi -> groundi -> groundi -> goal

    (** Relational multiplication *)
    val mulo  : groundi -> groundi -> groundi -> goal

    (** Infix syninym for [mulo] *)
    val ( * ) : groundi -> groundi -> groundi -> goal

    (** Comparisons *)
    val leo : groundi -> groundi -> Bool.groundi -> goal
    val geo : groundi -> groundi -> Bool.groundi -> goal
    val gto : groundi -> groundi -> Bool.groundi -> goal
    val lto : groundi -> groundi -> Bool.groundi -> goal

    (** Comparisons as goals *)
    val (<=) : groundi -> groundi -> goal
    val (>=) : groundi -> groundi -> goal
    val (>)  : groundi -> groundi -> goal
    val (<)  : groundi -> groundi -> goal
  end

(** [inj_nat n] is a deforested synonym for injection *)
val inj_nat : int -> Nat.groundi

(** {3 Relational Lists} *)
module List :
  sig
    (** {3 Standard list definitions} *)
    include module type of struct include List end

    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for abstract list type *)
    type ('a, 'l) t = ('a, 'l) llist

    (** Ground lists (isomorphic to regular ones) *)
    type 'a ground = ('a, 'a ground) t

    (** Logic lists (with the tails as logic lists) *)
    type 'a logic  = ('a, 'a logic) t logic'

    (** GT-compatible typeinfo for ['a ground] *)
    val ground :
      (unit,
       < gmap    : ('a -> 'b) -> 'a ground -> 'b ground;
         compare : ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
         foldl   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
         foldr   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
         html    : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer;
         show    : ('a -> string) -> 'a ground -> string >)
      GT.t

    (** GT-compatible typeinfo for ['a logic] *)
    val logic :
      (unit,
        < gmap    : ('a -> 'b) -> (('a, 'c) t logic' as 'c) -> (('b, 'd) t logic' as 'd);
          compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
          eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
          foldr   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
          foldl   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
          html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
          show    : ('a -> string) -> 'a logic -> GT.string  >)
        GT.t

    (** A synonym for injected list *)
    type ('a,'b) groundi = ('a ground, 'b logic) injected

    (** [of_list l] converts regular OCaml list [l] into isomorphic OCanren [ground] list *)
    val of_list : ('a -> 'b) -> 'a list -> 'b ground

    (** [to_list g] converts OCanren list [g] into regular OCaml list *)
    val to_list : ('a -> 'b) -> 'a ground -> 'b list

    (** [to_logic x] makes logic list from ground one *)
    val to_logic : ('a -> 'b) -> 'a ground -> 'b logic

    (** [from_logic x] makes ground list from logic one.
        Raises exception [Not_a_value] if [x] contains logic variables.*)
    val from_logic : ('b -> 'a) -> 'b logic -> 'a ground

    (** Make injected list from ground one *)
    val inj : ('a -> (('a, 'b) injected)) -> 'a ground -> ('a, 'b) groundi

    (** Reifier *)
    val reify : (helper -> ('a, 'b) injected -> 'b) -> helper -> ('a ground, 'b logic) injected -> 'b logic

    (** Constructor *)
    val conso : ('a, 'b) injected -> ('a, 'b) groundi -> ('a, 'b) groundi

    (** Relational foldr *)
    val foldro : (('a, 'b) injected -> ('acc, _ MiniKanrenCore.logic as 'acc2) injected -> ('acc, 'acc2) injected -> goal) ->
                 ('acc, 'acc2) injected -> ('a, 'b) groundi -> ('acc, 'acc2) injected -> goal

    (** Relational map *)
    val mapo : (('a, 'b) injected -> ('q, 'w) injected -> goal) -> ('a, 'b) groundi -> ('q, 'w) groundi -> goal

    (** Relational filter *)
    val filtero : (('a, 'b) injected -> Bool.groundi -> goal) -> ('a, 'b) groundi -> ('a, 'b) groundi -> goal

    (** Relational lookup *)
    val lookupo : (('a, 'b) injected -> Bool.groundi -> goal) -> ('a, 'b) groundi -> ('a option, 'b option MiniKanrenCore.logic) injected -> goal

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
    val membero : ('a, 'b MiniKanrenCore.logic) groundi  -> ('a, 'b MiniKanrenCore.logic) injected  -> goal

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

  end

(** [inj_list inj_a l] is a deforested synonym for injection *)
val inj_list : ('a -> ('a, 'b) injected) -> 'a list -> ('a, 'b) List.groundi

val inj_listi : ('a, 'b) injected list -> ('a, 'b) List.groundi

val pair   : ('a, 'b) injected -> ('c, 'd) injected -> ('a * 'c, ('b * 'd) logic) injected
val triple : ('a, 'd) injected -> ('b, 'e) injected -> ('c,'f) injected -> ('a * 'b * 'c, ('d * 'e * 'f) logic) injected

(** [inj_nat_list l] is a deforsted synonym for injection *)
val inj_nat_list : int list -> (Nat.ground, Nat.logic) List.groundi

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) injected -> ('a,'b) List.groundi -> ('a,'b) List.groundi

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) List.groundi

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) injected -> ('a, 'b) List.groundi

(** [nil] is a synonym for [inj Nil] *)
val nil : unit -> (_, _) List.groundi
