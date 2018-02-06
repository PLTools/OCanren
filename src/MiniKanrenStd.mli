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

(** {2 Standard relational library} *)

(** {3 Some predefined types} *)

(** Abstract list type *)
@type ('a, 'l) list =
| Nil
| Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr

(** Abstract nat type *)
@type 'a nat =
| O
| S of 'a with show, html, eq, compare, foldl, foldr, gmap

(** {3 Relational pairs} *)
module Pair :
  sig

    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for regular option type *)
    type ('a, 'b) t = 'a * 'b

    (** Ground option (the regular one) *)
    type ('a, 'b) ground = 'a * 'b

    (** Logic option *)
    type ('a, 'b) logic = ('a * 'b) logic'

    (** GT-compatible typeinfo for [ground] *)
    val ground :
      (unit,
       < compare : ('a -> 'a -> GT.comparison) -> ('b -> 'b -> GT.comparison) -> ('a, 'b) ground -> ('a, 'b) ground -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) ground -> ('a, 'b) ground -> bool;
         foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) ground -> 'c;
         foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) ground -> 'c;
         gmap    : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) ground -> ('c, 'd) ground;
         html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) -> ('a, 'b) ground -> HTML.viewer;
         show    : ('a -> string) -> ('b -> string) -> ('a, 'b) ground -> string >)
      GT.t

    (** GT-compatible typeinfo for [logic] *)
    val logic :
      (unit,
       < compare : ('a -> 'a -> GT.comparison) -> ('b -> 'b -> GT.comparison) -> ('a, 'b) logic -> ('a, 'b) logic -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) logic -> ('a, 'b) logic -> bool;
         foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) logic -> 'c;
         foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) logic -> 'c;
         gmap    : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) logic -> ('c, 'd) logic;
         html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) -> ('a, 'b) logic -> HTML.viewer;
         show    : ('a -> string) -> ('b -> string) -> ('a, 'b) logic -> string >)
      GT.t

    (** Logic injection (for reification) *)
    val inj : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) ground -> ('c, 'd) logic

    (** A synonym for injected pair *)
    type ('a, 'b, 'c, 'd) groundi = (('a, 'c) ground, ('b, 'd) logic) injected

    (** Make injected pair from ground one with injected components *)
    val pair : ('a, 'b) injected -> ('c, 'd) injected -> ('a, 'b, 'c, 'd) groundi

    (** Reifier *)
    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> Env.t -> ('a, 'b, 'c, 'd) groundi -> ('b, 'd) logic

  end

(** {3 Relational [option]} *)
module Option :
  sig

    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for regular option type *)
    type 'a t = 'a option

    (** Ground option (the regular one) *)
    type 'a ground = 'a option

    (** Logic option *)
    type 'a logic = 'a option logic'

    (** GT-compatible typeinfo for [ground] *)
    val ground :
      (unit,
       < compare : ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
         foldl   : ('a -> 'b -> 'a) -> 'a -> 'b ground -> 'a;
         foldr   : ('a -> 'b -> 'a) -> 'a -> 'b ground -> 'a;
         gmap    : ('a -> 'b) -> 'a ground -> 'b ground;
         html    : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer;
         show    : ('a -> string) -> 'a ground -> string >)
      GT.t

    (** GT-compatible typeinfo for [logic] *)
    val logic :
      (unit,
       < compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
         foldl   : ('a -> 'b -> 'a) -> 'a -> 'b logic -> 'a;
         foldr   : ('a -> 'b -> 'a) -> 'a -> 'b logic -> 'a;
         gmap    : ('a -> 'b) -> 'a logic -> 'b logic;
         html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
         show    : ('a -> string) -> 'a logic -> string >)
      GT.t

    (** Logic injection (for reification) *)
    val inj : ('a -> 'b) -> 'a ground -> 'b logic

    (** A synonym for injected option *)
    type ('a, 'b) groundi = ('a ground, 'b logic) injected

    (** Make injected [option] from ground one with injected value *)
    val option : ('a, 'b) injected ground -> ('a, 'b) groundi

    (** Reifier *)
    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> Env.t -> ('a, 'b) groundi -> 'b logic

    (** Injection counterpart for constructors *)
    val some : ('a, 'b) injected -> ('a, 'b) groundi
    val none : unit -> ('a, 'b) groundi

  end

(** {3 Relational booleans} *)
module Bool :
  sig
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

    (** Logic injection (for reification) *)
    val inj : ground -> logic

    (** A synonym for injected boolean; use [(!!)] operator to make a [groundi] from a regular [bool] *)
    type groundi = (ground, logic) injected

    (** Reifier *)
    val reify : Env.t -> groundi -> logic

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
  end

(** Equality as boolean relation *)
val eqo : ('a, 'b logic) injected -> ('a, 'b logic) injected -> Bool.groundi -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b logic) injected -> ('a, 'b logic) injected -> Bool.groundi -> goal

(** {3 Relational numbers} *)
module Nat :
  sig
    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for abstract nat type *)
    type 'a t = 'a nat

    (** Ground nat are ismorphic for regular one *)
    type ground = ground t

    (** Logic nat *)
    type logic = logic t logic'

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

    (** Logic injection (for reification) *)
    val inj : ground -> logic

    (** A type synonym for injected nat *)
    type groundi = (ground, logic) injected

    (** Reifier *)
    val reify : Env.t -> groundi -> logic

    (** [of_int n] converts integer [n] into [ground]; negative integers become [O] *)
    val of_int : int -> ground

    (** [to_int g] converts ground [g] into integer *)
    val to_int : ground -> int

    (** Make injected [nat] from ground one *)
    val nat : ground -> groundi

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
val nat : int -> Nat.groundi

(** {3 Relational Lists} *)
module List :
  sig
    (** {3 Standard list definitions} *)
    include module type of struct include List end

    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for abstract list type *)
    type ('a, 'l) t = ('a, 'l) list

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
    val of_list : ('a -> 'b) -> 'a GT.list -> 'b ground

    (** [to_list g] converts OCanren list [g] into regular OCaml list *)
    val to_list : ('a -> 'b) -> 'a ground -> 'b GT.list

    (** [inj x] makes a logic list from a ground one *)
    val inj : ('a -> 'b) -> 'a ground -> 'b logic

    (** Make injected [list] from ground one of injected elements *)
    val list : ('a, 'b) injected GT.list -> ('a, 'b) groundi

    (** Reifier *)
    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> Env.t -> ('a ground, 'b logic) injected -> 'b logic

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

  end

(** [inj_list inj_a l] is a deforested synonym for injection *)
val list : ('a -> ('a, 'b) injected) -> 'a GT.list -> ('a, 'b) List.groundi

(** [inj_nat_list l] is a deforsted synonym for injection *)
val nat_list : int GT.list -> (Nat.ground, Nat.logic) List.groundi

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) injected -> ('a,'b) List.groundi -> ('a,'b) List.groundi

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) List.groundi

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) injected -> ('a, 'b) List.groundi

(** [nil] is a synonym for [inj Nil] *)
val nil : unit -> (_, _) List.groundi

(** Synonyms for [option] constructors *)
val some : ('a, 'b) injected -> ('a, 'b) Option.groundi
val none : unit -> ('a, 'b) Option.groundi

(** Synonym for pair *)
val pair : ('a, 'b) injected -> ('c, 'd) injected -> ('a, 'b, 'c, 'd) Pair.groundi
