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

    (** [retrieve ~n:n s] returns the list of [n]-first elements of [s] and the rest of the stream *)
    val retrieve : ?n:int -> 'a t -> 'a list * 'a t

    (** [take ~n:n s] returns the list of [n]-first elements of [s] *)
    val take : ?n:int -> 'a t -> 'a list

    (** [map f s] maps function [f] over the stream [s] *)
    val map : ('a -> 'b) -> 'a t -> 'b t

  end

(** Type of typed logic variable *)
type var

(** Type of logic values: free variable with the list of disequality constraints or some value *)
@type 'a logic = private 
  Var   of var * 'a logic GT.list 
| Value of 'a                     
with show, html, eq, compare, foldl, foldr, gmap

(** Lifting primitive *)
val (!) : 'a -> 'a logic

(** Type of logic lists *)
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

(** Goal converts a state into lazy stream of states *)
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

(** {2 Combinators to produce fresh variables} *)
module Fresh :
  sig

    (** [succ num f] increments the number of free logic variables in
        a goal; can be used to get rid of ``fresh'' syntax extension *)
    val succ : ('a -> State.t -> 'b) -> ('c logic -> 'a) -> State.t -> 'b

    (** Zero logic parameters *)
    val zero : 'a -> 'a
 
    (** {3 One to five logic parameter(s)} *)

    val one   : ('a logic ->                                                 State.t -> 'b) -> State.t -> 'b
    val two   : ('a logic -> 'b logic ->                                     State.t -> 'c) -> State.t -> 'c
    val three : ('a logic -> 'b logic -> 'c logic ->                         State.t -> 'd) -> State.t -> 'd
    val four  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             State.t -> 'e) -> State.t -> 'e
    val five  : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'f
 
    (** {3 One to five logic parameter(s), conventional names} *)

    val q     : ('a logic ->                                                 State.t -> 'b) -> State.t -> 'b
    val qr    : ('a logic -> 'b logic ->                                     State.t -> 'c) -> State.t -> 'c
    val qrs   : ('a logic -> 'b logic -> 'c logic ->                         State.t -> 'd) -> State.t -> 'd
    val qrst  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             State.t -> 'e) -> State.t -> 'e
    val pqrst : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'f
 
  end

(** {2 Top-level running primitives} *)

(** [run n g h] runs a goal [g] with [n] logical parameters and passes refined
    results to the handler [h]. The number of parameters is encoded using variadic
    machinery {a la} Danvy and represented by a number of predefined numerals and
    successor function (see below). The refinement replaces each variable, passed
    to [g], with the stream of values, associated with that variables as the goal
    succeeds. 
     
    Examples:

    - [run one        (fun q   -> q === !5)              (fun qs    -> ]{i here [q]s     --- a stream of all values, associated with the variable [q]}[)]
    - [run two        (fun q r -> q === !5 ||| r === !6) (fun qs rs -> ]{i here [qs], [rs] --- streams of all values, associated with the variable [q] and [r], respectively}[)]
    - [run (succ one) (fun q r -> q === !5 ||| r === !6) (fun qs rs -> ]{i the same as the above}[)]
 *)
val run : (unit -> ('a -> State.t -> 'c) * ('d -> 'e -> 'f) * (('g -> 'h -> 'e) * ('c -> 'h * 'g))) -> 'a -> 'd -> 'f

(** Some type to refine a stream of states into the stream of answers (w.r.t. some known
    logic variable *)
type 'a refiner = State.t Stream.t -> 'a logic Stream.t

(** Successor function *)
val succ :
  (unit -> ('a -> State.t -> 'b) * ('c -> 'd -> 'e) * (('f -> 'g -> 'h) * ('i -> 'j * 'k))) -> 
  (unit -> (('l logic -> 'a) -> State.t -> 'l refiner * 'b) * (('m -> 'c) -> 'm * 'd -> 'e) * (('f -> ('f -> 'n) * 'g -> 'n * 'h) * ('o * 'i -> ('o * 'j) * 'k)))

(** {3 Predefined numerals (one to five)} *)

val one :
  unit ->
  (('a logic -> State.t -> 'b) -> State.t -> 'a refiner * 'b) *
  (('c -> 'd) -> 'c -> 'd) * 
  (('e -> ('e -> 'f) -> 'f) * ('g -> 'g))

val two :
  unit ->
  (('b logic -> 'c logic -> State.t -> 'd) -> State.t -> 'b refiner * ('c refiner * 'd)) *
  (('e -> 'f -> 'g) -> 'e * 'f -> 'g) * 
  (('h -> ('h -> 'i) * ('h -> 'j) -> 'i * 'j) * ('k * ('l * 'm) -> ('k * 'l) * 'm))
	    
val three :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> State.t -> 'e) -> State.t -> 'b refiner * ('c refiner * ('d refiner * 'e))) *
  (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
  (('j -> ('j -> 'k) * (('j -> 'l) * ('j -> 'm)) -> 'k * ('l * 'm)) * ('n * ('o * ('p * 'q)) -> ('n * ('o * 'p)) * 'q))
	    
val four :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'b refiner * ('c refiner * ('d refiner * ('e refiner * 'f)))) *
  (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * (('l -> 'o) * ('l -> 'p))) -> 'm * ('n * ('o * 'p))) * ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u))
	    
val five :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> 'f logic -> State.t -> 'g) -> State.t ->	'b refiner * ('c refiner * ('d refiner * ('e refiner * ('f refiner * 'g))))) *
  (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) * 
  (('n -> ('n -> 'o) * (('n -> 'p) * (('n -> 'q) * (('n -> 'r) * ('n -> 's)))) -> 'o * ('p * ('q * ('r * 's)))) * ('t * ('u * ('v * ('w * ('x * 'y)))) -> ('t * ('u * ('v * ('w * 'x)))) * 'y))

(** {3 The same numerals with conventional names} *)

val q :
  unit ->
  (('a logic -> State.t -> 'b) -> State.t -> 'a refiner * 'b) *
  (('c -> 'd) -> 'c -> 'd) * 
  (('e -> ('e -> 'f) -> 'f) * ('g -> 'g))

val qr :
  unit ->
  (('b logic -> 'c logic -> State.t -> 'd) -> State.t -> 'b refiner * ('c refiner * 'd)) *
  (('e -> 'f -> 'g) -> 'e * 'f -> 'g) * 
  (('h -> ('h -> 'i) * ('h -> 'j) -> 'i * 'j) * ('k * ('l * 'm) -> ('k * 'l) * 'm))
	    
val qrs :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> State.t -> 'e) -> State.t -> 'b refiner * ('c refiner * ('d refiner * 'e))) *
  (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
  (('j -> ('j -> 'k) * (('j -> 'l) * ('j -> 'm)) -> 'k * ('l * 'm)) * ('n * ('o * ('p * 'q)) -> ('n * ('o * 'p)) * 'q))
	    
val qrst :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> State.t -> 'f) -> State.t -> 'b refiner * ('c refiner * ('d refiner * ('e refiner * 'f)))) *
  (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * (('l -> 'o) * ('l -> 'p))) -> 'm * ('n * ('o * 'p))) * ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u))
	    
val pqrst :
  unit ->
  (('b logic -> 'c logic -> 'd logic -> 'e logic -> 'f logic -> State.t -> 'g) -> State.t ->	'b refiner * ('c refiner * ('d refiner * ('e refiner * ('f refiner * 'g))))) *
  (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) * 
  (('n -> ('n -> 'o) * (('n -> 'p) * (('n -> 'q) * (('n -> 'r) * ('n -> 's)))) -> 'o * ('p * ('q * ('r * 's)))) * ('t * ('u * ('v * ('w * ('x * 'y)))) -> ('t * ('u * ('v * ('w * 'x)))) * 'y))

