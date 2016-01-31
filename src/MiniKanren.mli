(*
 * MiniKanren: miniKanren primitives implementation.
 * Copyright (C) 2015
 * Dmitri Boulytchev, Dmitry Kosarev, St.Petersburg State University
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

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

(** Type of typed logic variable *)
@type 'a logic = private Var of GT.int | Value of 'a with show, html, eq, compare, foldl, foldr, gmap

(** Lifting primitive *)
val (!) : 'a -> 'a logic

(** Type of ligic lists *)
@type 'a llist = Nil | Cons of 'a logic * 'a llist logic with show, html, eq, compare, foldl, foldr, gmap

(** Infix synonym for [Cons] *)
val (%) : 'a logic -> 'a llist logic -> 'a llist logic

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : 'a logic -> 'a logic -> 'a llist logic

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : 'a logic -> 'a llist logic

(** [of_list l] converts a regular list into logic one *)
val of_list : 'a list -> 'a llist logic

(** [to_listk k l] converts logic list [l] into a regular one, calling [k] to
    convert elements, which are not a value *)
val to_listk : ('a llist logic -> 'a list) -> 'a llist logic -> 'a list

(** Exception to raise on a non-value case *)
exception Not_a_value

(** [to_value x] converts logic into value; raises [Not_a_value] on a
    non-value case
*)
val to_value : 'a logic -> 'a

(** [to_list l] converts logic list [l] into a regular one, raising
    [Not_a_value] on a non-value case *)
val to_list : 'a llist logic -> 'a list

(** State (needed to perform calculations) *)
module State :
  sig
    (** State type *)
    type t

    (** Printing helper *)
    val show : t -> string
  end

(** Lazy streams *)
module Stream :
  sig

    (** Type of the stream *)
    type 'a t

    (** Lazy constructor *)
    val from_fun : (unit -> 'a t) -> 'a t
  end

(** Goal converts a state into a lazy stream of states *)
type goal = State.t -> State.t Stream.t

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : ('a logic -> State.t -> 'b) -> State.t -> 'b

(** [succ num f] increments the number of free logic variables in
    a goal; can be used to get rid of ``fresh'' syntax extension *)
val succ : ('a -> State.t -> 'b) -> ('c logic -> 'a) -> State.t -> 'b

(** Zero logic parameters *)
val zero : 'a -> 'a

(** One to five logic parameter(s) *)
val one   : ('a logic ->                                                 State.t -> 'b) -> State.t -> 'b
val two   : ('a logic -> 'b logic ->                                     State.t -> 'c) -> State.t -> 'c
val three : ('a logic -> 'b logic -> 'c logic ->                         State.t -> 'd) -> State.t -> 'd
val four  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             State.t -> 'e) -> State.t -> 'e
val five  : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'f

(** One to five logic parameter(s), conventional names *)
val q     : ('a logic ->                                                 State.t -> 'b) -> State.t -> 'b
val qr    : ('a logic -> 'b logic ->                                     State.t -> 'c) -> State.t -> 'c
val qrs   : ('a logic -> 'b logic -> 'c logic ->                         State.t -> 'd) -> State.t -> 'd
val qrst  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             State.t -> 'e) -> State.t -> 'e
val pqrst : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'f

(** [x === y] creates a goal, which performs a unifications of
    [x] and [y] *)
val (===) : 'a logic -> 'a logic -> goal

(** [x === y] creates a goal, which performs a non-unification check for
    [x] and [y] *)
val (=/=) : 'a logic -> 'a logic -> goal

(** [conj s1 s2] creates a goal, which is a conjunction of its arguments *)
val conj : goal -> goal -> goal

(** [&&&] is left-associative infix synonym for [conj] *)
val (&&&) : goal -> goal -> goal

(** [disj s1 s2] creates a goal, which is a disjunction of its arguments *)
val disj : goal -> goal -> goal

(** [|||] is left-associative infix synonym for [disj] *)
val (|||) : goal -> goal -> goal

(** [?| [s1; s2; ...; sk]] calculates [s1 ||| s2 ||| ... ||| sk] for a
    non-empty list of goals *)
val (?|) : goal list -> goal

(** [conde] is a synonym for [?|] *)
val conde : goal list -> goal

(** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a
    non-empty list of goals *)
val (?&) : goal list -> goal

(** {2 Top-level running primitives} *)

(** [run s] runs a state transformer [s] (not necessarily a goal) in
    initial state *)
val run : (State.t -> 'a) -> 'a

(** [diseq] is a type for disequality constraint *)
type diseq

(** [refine s x] refines a logical variable [x] (created with [fresh]) w.r.t.
    state [s] *)
val refine : State.t -> 'a logic -> 'a logic * diseq

(** [reify s x] reifies disequality constraint for a given logic variable; the result
    is a list of logic expressions, which given variable should not be equal to *)
val reify : diseq -> 'a logic -> 'a logic list

(** [take ?(n=k) s] takes at most [k] first answers from the lazy
    stream [s] (reexported from MKStream for convenience) *)
val take : ?n:int -> State.t Stream.t -> State.t list

(** Exception to raise on infinine unification result *)
exception Occurs_check
