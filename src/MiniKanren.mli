(*
 * MiniKanren: miniKanren primitives implementation.
 * Copyright (C) 2015-2016
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

(** Lazy streams *)
module Stream :
  sig

    (** Type of the stream *)
    type 'a t

    (** Lazy constructor *)
    val from_fun : (unit -> 'a t) -> 'a t

  end

(** Type of typed logic variable *)
type var

@type 'a logic = private Var of var * 'a logic GT.list | Value of 'a with show, html, eq, compare, foldl, foldr, gmap

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

(** Goal converts a state into a lazy stream of states *)
type goal = State.t -> State.t Stream.t

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : ('a logic -> State.t -> 'b) -> State.t -> 'b

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

type 'a reifier = State.t Stream.t -> 'a logic Stream.t

val one :
  unit ->
  (('a logic -> State.t -> 'b) -> State.t -> 'a reifier * 'b) *
  (('c -> 'd) -> 'c -> 'd) * 
  (('e -> ('e -> 'f) -> 'f) * ('g -> 'g))

val succ :
  (unit -> ('a -> State.t -> 'b) * ('c -> 'd -> 'e) * (('f -> 'g -> 'h) * ('i -> 'j * 'k))) -> 
  (unit -> (('l logic -> 'a) -> State.t -> 'l reifier * 'b) * (('m -> 'c) -> 'm * 'd -> 'e) * (('f -> ('f -> 'n) * 'g -> 'n * 'h) * ('o * 'i -> ('o * 'j) * 'k)))

val two :
  unit ->
  (('b logic -> 'c logic -> State.t -> 'd) -> State.t -> 'b reifier * ('c reifier * 'd)) *
  (('e -> 'f -> 'g) -> 'e * 'f -> 'g) * 
  (('h -> ('h -> 'i) * ('h -> 'j) -> 'i * 'j) * ('k * ('l * 'm) -> ('k * 'l) * 'm))
	    
val three :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> State.t -> 'e) -> State.t -> 'b reifier * ('c reifier * ('d reifier * 'e))) *
  (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
  (('j -> ('j -> 'k) * (('j -> 'l) * ('j -> 'm)) -> 'k * ('l * 'm)) * ('n * ('o * ('p * 'q)) -> ('n * ('o * 'p)) * 'q))
	    
val four :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'b reifier * ('c reifier * ('d reifier * ('e reifier * 'f)))) *
  (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * (('l -> 'o) * ('l -> 'p))) -> 'm * ('n * ('o * 'p))) * ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u))
	    
val five :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> 'f logic -> State.t -> 'g) -> State.t ->	'b reifier * ('c reifier * ('d reifier * ('e reifier * ('f reifier * 'g))))) *
  (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) * 
  (('n -> ('n -> 'o) * (('n -> 'p) * (('n -> 'q) * (('n -> 'r) * ('n -> 's)))) -> 'o * ('p * ('q * ('r * 's)))) * ('t * ('u * ('v * ('w * ('x * 'y)))) -> ('t * ('u * ('v * ('w * 'x)))) * 'y))

val q :
  unit ->
  (('a logic -> State.t -> 'b) -> State.t -> 'a reifier * 'b) *
  (('c -> 'd) -> 'c -> 'd) * 
  (('e -> ('e -> 'f) -> 'f) * ('g -> 'g))

val qr :
  unit ->
  (('b logic -> 'c logic -> State.t -> 'd) -> State.t -> 'b reifier * ('c reifier * 'd)) *
  (('e -> 'f -> 'g) -> 'e * 'f -> 'g) * 
  (('h -> ('h -> 'i) * ('h -> 'j) -> 'i * 'j) * ('k * ('l * 'm) -> ('k * 'l) * 'm))
	    
val qrs :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> State.t -> 'e) -> State.t -> 'b reifier * ('c reifier * ('d reifier * 'e))) *
  (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
  (('j -> ('j -> 'k) * (('j -> 'l) * ('j -> 'm)) -> 'k * ('l * 'm)) * ('n * ('o * ('p * 'q)) -> ('n * ('o * 'p)) * 'q))
	    
val qrst :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'b reifier * ('c reifier * ('d reifier * ('e reifier * 'f)))) *
  (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * (('l -> 'o) * ('l -> 'p))) -> 'm * ('n * ('o * 'p))) * ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u))
	    
val pqrst :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> 'f logic -> State.t -> 'g) -> State.t ->	'b reifier * ('c reifier * ('d reifier * ('e reifier * ('f reifier * 'g))))) *
  (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) * 
  (('n -> ('n -> 'o) * (('n -> 'p) * (('n -> 'q) * (('n -> 'r) * ('n -> 's)))) -> 'o * ('p * ('q * ('r * 's)))) * ('t * ('u * ('v * ('w * ('x * 'y)))) -> ('t * ('u * ('v * ('w * 'x)))) * 'y))

val run :
  (unit -> ('a -> State.t -> 'c) * ('d -> 'e -> 'f) * (('g -> 'h -> 'e) * ('c -> 'h * 'g))) -> 'a -> 'd -> 'f

