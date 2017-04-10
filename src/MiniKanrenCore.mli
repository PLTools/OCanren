(*
 * MiniKanren: miniKanren implementation.
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

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

module Stream :
  sig
    (** Internal stream type *)
    type 'a internal

    (** Stream type *)
    type 'a t

    (** Emptiness test *)
    val is_empty : 'a t -> bool

    (** Constructor *)
    val from_fun : (unit -> 'a t) -> 'a t

    (** [retrieve ~n:n s] returns the list of [n]-first elements of [s] and the rest of the stream *)
    val retrieve : ?n:int -> 'a t -> 'a list * 'a t

    (** [take ~n:n s] returns the list of [n]-first elements of [s] *)
    val take : ?n:int -> 'a t -> 'a list

    (** [hd s] gets a head of the stream *)
    val hd : 'a t -> 'a

    (** [hd s] gets a tail of the stream *)
    val tl : 'a t -> 'a t

    (** [map f s] maps function [f] over the stream [s] *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** [iter f s] iterates function [f] over the stream [s] *)
    val iter : ('a -> unit) -> 'a t -> unit

    val zip : 'a t -> 'b t -> ('a * 'b) t
  end

(** {3 States and goals} *)

(** A state *)
module State :
  sig
    (** State type *)
    type t
  end

(** Goal converts a state into a lazy stream of states *)
type 'a goal' = State.t -> 'a
type goal = State.t Stream.internal goal'

(** {3 Logic values} *)

(** A type of a logic value *)
@type 'a logic = private
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

(** GT-compatible typeinfo for logics *)
val logic :
  (unit,
   < show    : ('a -> string) -> 'a logic -> string;
     html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
     eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
     compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
     foldl   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     foldr   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     gmap    : ('a -> 'sa) -> 'a logic -> 'sa logic
   >) GT.t

(** [to_logic x] makes a logic value from a regular one *)
val to_logic : 'a -> 'a logic

(** [from_logic x] makes a regular value from a logic one.
    Raises exception [Not_a_value] if [x] contains free variables
*)
val from_logic : 'a logic -> 'a

(** {3 Injections/projections} *)

(**  The type [('a, 'b) injected] describes an injection of a type ['a] into ['b] *)
type ('a, 'b) injected

(** [lift x] lifts [x] into injected doamin *)
val lift : 'a -> ('a, 'a) injected

(** [inj x] injects [x] into logical [x] *)
val inj : ('a, 'b) injected -> ('a, 'b logic) injected

(** A synonym for [fun x -> inj @@ lift x] (for non-parametric types) *)
val (!!) : 'a -> ('a, 'a logic) injected

(** [prj x] returns a regular value from injected representation.
    Raises exception [Not_a_value] if [x] contains free variables
*)
val prj : ('a, 'b) injected -> 'a

(** {3 miniKanren basic combinators} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'b) injected -> goal) -> goal

(** [x === y] creates a goal, which performs a unification of [x] and [y] *)
val (===) : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for [x] and [y] *)
val (=/=) : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(** [conj s1 s2] creates a goal, which is a conjunction of its arguments *)
val conj : goal -> goal -> goal

(** [&&&] is a left-associative infix synonym for [conj] *)
val (&&&) : goal -> goal -> goal

(** [disj s1 s2] creates a goal, which is a disjunction of its arguments *)
val disj : goal -> goal -> goal

(** [|||] is a left-associative infix synonym for [disj] *)
val (|||) : goal -> goal -> goal

(** [?| [s1; s2; ...; sk]] calculates [s1 ||| (s2 ||| ... ||| sk)...)] for a non-empty list of goals
    (note the {i right} association)
*)
val (?|) : goal list -> goal

(** [conde] is a synonym for [?|] *)
val conde : goal list -> goal

(** [?& [s1; s2; ...; sk]] calculates [s1 &&& (s2 && ... &&& sk)...)] for a non-empty list of goals
    (note the {i right} association)
*)
val (?&) : goal list -> goal

(** {2 Some predefined goals} *)

(** [success] always succeeds *)
val success : goal

(** [failure] always fails *)
val failure : goal

(** {2 Combinators to produce fresh variables} *)
module Fresh :
  sig
    (** [succ num f] increments the number of free logic variables in
        a goal; can be used to get rid of ``fresh'' syntax extension
    *)
    val succ : ('a -> 'b goal') -> ((_, _) injected -> 'a) -> 'b goal'

    (** Zero logic parameters *)
    val zero : 'a -> 'a

    (** {3 One to five logic parameter(s)} *)
    val one   : (_ injected ->                                                         goal) -> goal
    val two   : (_ injected -> _ injected ->                                           goal) -> goal
    val three : (_ injected -> _ injected -> _ injected ->                             goal) -> goal
    val four  : (_ injected -> _ injected -> _ injected -> _ injected ->               goal) -> goal
    val five  : (_ injected -> _ injected -> _ injected -> _ injected -> _ injected -> goal) -> goal

    (** {3 One to five logic parameter(s), conventional names} *)
    val q     : (_ injected ->                                                         goal) -> goal
    val qr    : (_ injected -> _ injected ->                                           goal) -> goal
    val qrs   : (_ injected -> _ injected -> _ injected ->                             goal) -> goal
    val qrst  : (_ injected -> _ injected -> _ injected -> _ injected ->               goal) -> goal
    val pqrst : (_ injected -> _ injected -> _ injected -> _ injected -> _ injected -> goal) -> goal
  end

(** {2 Top-level running primitives} *)

(** [run n g h] runs a goal [g] with [n] logical parameters and passes reified results to the handler [h].
    The number of parameters is encoded using variadic machinery {a la} Olivier Danvy and represented by
    a number of predefined numerals and successor function (see below). The reification replaces each variable,
    passed to [g], with the stream of values, associated with that variable as the goal succeeds.

    Examples:

    - [run one        (fun q   -> q === !!5)               (fun qs    -> ]{i here [q]s       --- a stream of all values, associated with the variable [q]}[)]
    - [run two        (fun q r -> q === !!5 ||| r === !!6) (fun qs rs -> ]{i here [qs], [rs] --- streams of all values, associated with the variable [q] and [r], respectively}[)]
    - [run (succ one) (fun q r -> q === !!5 ||| r === !!6) (fun qs rs -> ]{i the same as the above}[)]
*)
val run : (unit -> ('a -> 'c goal') * ('d -> 'e -> 'f) * (State.t Stream.t -> 'h -> 'e) * ('c -> 'h * State.t Stream.internal)) ->
          'a -> 'd -> 'f

(** The primitive [delay] helps to construct recursive goals, which depend on themselves. For example,
    we can't write [let rec fives q = (q === !!5) ||| (fives q)] because the generation of this goal leads to
    infinite recursion. The correct way to implement this is [let rec fives q = (q === !!5) ||| delay (fun () -> fives q)]

    See also syntax extension [defer].
*)
val delay : (unit -> goal) -> goal

(** Reification helper *)
type helper

(** The exception is raised when we try to extract a regular term from the answer with some free variables *)
exception Not_a_value

(** Reification result *)
class type ['a,'b] reified =
object
  (** Returns [true] if the term has any free logic variable inside *)
  method is_open: bool

  (** Gets the answer as regular term. Raises exception [Not_a_value] when the answer contains free variables *)
  method prj: 'a

  (** Gets the answer as a logic value using provided injection function [inj] *)
  method reify: (helper -> ('a, 'b) injected -> 'b) -> 'b
end

(** Successor function *)
val succ : (unit ->
            ('a -> 'b goal') * ('c -> 'd -> 'e) * (State.t Stream.t -> 'f -> 'g) * ('h -> 'i * 'j)) ->
           unit ->
           ((('k, 'l) injected -> 'a) -> (('k, 'l) injected * 'b) goal') *
           (('m -> 'c) -> 'm * 'd -> 'e) *
           (State.t Stream.t -> ('n, 'o) injected * 'f -> ('n, 'o) reified Stream.t * 'g) *
           ('p * 'h -> ('p * 'i) * 'j)

(** {3 Predefined numerals (one to five)} *)
val one : unit ->
  ((('a,'c) injected -> State.t Stream.internal goal') -> (('a, 'c) injected * State.t Stream.internal) goal') *
  (('d -> 'e) -> 'd -> 'e) *
  (State.t Stream.t ->
    ('f, 'g) injected -> ('f, 'g) reified Stream.t) *
  ('h -> 'h)

val two : unit ->
  ((('a,'d) injected -> ('b,'e) injected -> State.t Stream.internal goal') ->
    (('a, 'd) injected * (('b, 'e) injected * State.t Stream.internal)) goal') *
  (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
  (State.t Stream.t ->
    ('i, 'j) injected * ('k, 'l) injected ->
    ('i, 'j) reified Stream.t *
    ('k, 'l) reified Stream.t) *
  ('m * ('n * 'o) -> ('m * 'n) * 'o)

val three : unit ->
  ((('a,'e) injected -> ('b, 'f) injected -> ('c, 'g) injected -> State.t Stream.internal goal') ->
    (('a, 'e) injected * (('b, 'f) injected * (('c, 'g) injected * State.t Stream.internal))) goal') *
  (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
  (State.t Stream.t ->
    ('l, 'm) injected * (('n, 'o) injected * ('p, 'q) injected) ->
    ('l, 'm) reified Stream.t *
    (('n, 'o) reified Stream.t *
    ('p, 'q) reified Stream.t)) *
  ('r * ('s * ('t * 'u)) -> ('r * ('s * 't)) * 'u)

val four : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> State.t Stream.internal goal') ->
    (('a, 'g) injected * (('b, 'h) injected * (('c, 'i) injected * (('d, 'j) injected * State.t Stream.internal)))) goal') *
  (('l -> 'm -> 'n -> 'o -> 'q) ->
    'l * ('m * ('n * 'o)) -> 'q) *
  (State.t Stream.t ->
     ('r, 's) injected * (('t, 'u) injected * (('v, 'w) injected * ('x, 'y) injected)) ->
     ('r, 's) reified Stream.t *
     (('t, 'u) reified Stream.t *
      (('v, 'w) reified Stream.t *
       (('x, 'y) reified Stream.t)))) *
  ('b1 * ('c1 * ('d1 * ('e1 * 'f1))) -> ('b1 * ('c1 * ('d1 * 'e1)))  * 'f1)

val five : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> ('e, 'k) injected -> State.t Stream.internal goal') ->
   (('a, 'g) injected *
    (('b, 'h) injected *
     (('c, 'i) injected * (('d, 'j) injected * (('e, 'k) injected * State.t Stream.internal))))) goal') *
  (('l -> 'm -> 'n -> 'o -> 'p -> 'q) ->
    'l * ('m * ('n * ('o * 'p))) -> 'q) *
  (State.t Stream.t ->
     ('r, 's) injected *
     (('t, 'u) injected *
      (('v, 'w) injected * (('x, 'y) injected * ('z, 'a1) injected))) ->
     ('r, 's) reified Stream.t *
     (('t, 'u) reified Stream.t *
      (('v, 'w) reified Stream.t *
       (('x, 'y) reified Stream.t *
        ('z, 'a1) reified Stream.t)))) *
  ('b1 * ('c1 * ('d1 * ('e1 * ('f1 * 'g1)))) -> ('b1 * ('c1 * ('d1 * ('e1 * 'f1)))) * 'g1)

(** {3 The same numerals with conventional names} *)
val q : unit ->
  ((('a,'c) injected -> State.t Stream.internal goal') -> (('a, 'c) injected * State.t Stream.internal) goal') *
  (('d -> 'e) -> 'd -> 'e) *
  (State.t Stream.t ->
    ('f, 'g) injected -> ('f, 'g) reified Stream.t) *
  ('h -> 'h)

val qr : unit ->
  ((('a,'d) injected -> ('b,'e) injected -> State.t Stream.internal goal') ->
    (('a, 'd) injected * (('b, 'e) injected * State.t Stream.internal)) goal') *
  (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
  (State.t Stream.t ->
    ('i, 'j) injected * ('k, 'l) injected ->
    ('i, 'j) reified Stream.t *
    ('k, 'l) reified Stream.t) *
  ('m * ('n * 'o) -> ('m * 'n) * 'o)

val qrs : unit ->
  ((('a,'e) injected -> ('b, 'f) injected -> ('c, 'g) injected -> State.t Stream.internal goal') ->
    (('a, 'e) injected * (('b, 'f) injected * (('c, 'g) injected * State.t Stream.internal))) goal') *
  (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
  (State.t Stream.t ->
    ('l, 'm) injected * (('n, 'o) injected * ('p, 'q) injected) ->
    ('l, 'm) reified Stream.t *
    (('n, 'o) reified Stream.t *
    ('p, 'q) reified Stream.t)) *
  ('r * ('s * ('t * 'u)) -> ('r * ('s * 't)) * 'u)

val qrst : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> State.t Stream.internal goal') ->
    (('a, 'g) injected * (('b, 'h) injected * (('c, 'i) injected * (('d, 'j) injected * State.t Stream.internal)))) goal') *
  (('l -> 'm -> 'n -> 'o -> 'q) ->
    'l * ('m * ('n * 'o)) -> 'q) *
  (State.t Stream.t ->
   ('r, 's) injected * (('t, 'u) injected * (('v, 'w) injected * ('x, 'y) injected)) ->
   ('r, 's) reified Stream.t *
   (('t, 'u) reified Stream.t *
    (('v, 'w) reified Stream.t *
     (('x, 'y) reified Stream.t)))) *
   ('b1 * ('c1 * ('d1 * ('e1 * 'f1))) -> ('b1 * ('c1 * ('d1 * 'e1)))  * 'f1)

module Tabling :
  sig
    val succ : (unit ->
                (('a -> 'b) -> 'c) * ('d -> 'e -> 'f) * ('h -> 'i * 'j)) ->
               unit ->
               (('k * 'a -> 'b) -> 'k -> 'c) *
               (('m -> 'd) -> 'm * 'e -> 'f) *
               ('p * 'h -> ('p * 'i) * 'j)

    val one : unit ->
      (('a * 'b -> 'c) -> 'a -> 'b -> 'c) *
      (('d -> 'e) -> 'd -> 'e) *
      ('h -> 'h)

    val two : unit ->
      (('a * ('b * 'c) -> 'd) -> 'a -> 'b -> 'c -> 'd) *
      (('e -> 'f -> 'g) -> 'e * 'f -> 'g) *
      ('m * ('n * 'o) -> ('m * 'n) * 'o)

    val three : unit ->
      (('a * ('b * ('c * 'd)) -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e) *
      (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
      ('r * ('s * ('t * 'u)) -> ('r * ('s * 't)) * 'u)

    val four : unit ->
      (('a * ('b * ('c * ('d * 'e))) -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f) *
      (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
      ('b1 * ('c1 * ('d1 * ('e1 * 'f1))) -> ('b1 * ('c1 * ('d1 * 'e1)))  * 'f1)

    val five : unit ->
      (('a * ('b * ('c * ('d * ('e * 'f)))) -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) *
      (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) *
      ('b1 * ('c1 * ('d1 * ('e1 * ('f1 * 'g1)))) -> ('b1 * ('c1 * ('d1 * ('e1 * 'f1)))) * 'g1)

    val tabled : (unit -> (('a -> State.t Stream.internal) -> 'b) * ('c -> 'd -> goal) * ('a -> 'd * State.t)) -> 'c -> 'b
  end

(** {2 Building reifiers for a custom type compositionally} *)
module type T1 =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type T2 =
  sig
   type ('a, 'b) t
   val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end

module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
  end

module type T4 =
  sig
    type ('a, 'b, 'c, 'd) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
  end

module type T5 =
  sig
    type ('a, 'b, 'c, 'd, 'e) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
  end

module type T6 =
  sig
    type ('a, 'b, 'c, 'd, 'e, 'f) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

module Fmap (T : T1) :
  sig
    val distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected
    val reify : (helper -> ('a,'b) injected -> 'b) -> helper -> ('a T.t, 'b T.t logic as 'r) injected -> 'r
  end

module Fmap2 (T : T2) :
  sig
    val distrib : (('a,'c) injected, ('b,'d) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected
    val reify : (helper -> ('a, 'b) injected -> 'b) -> (helper -> ('c, 'd) injected -> 'd) -> helper -> (('a, 'c) T.t, ('b, 'd) T.t logic as 'r) injected -> 'r
  end

module Fmap3 (T : T3) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected
    val reify : (helper -> ('a, 'b) injected -> 'b) -> (helper -> ('c, 'd) injected -> 'd) -> (helper -> ('e, 'f) injected -> 'f) ->
                helper -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic as 'r) injected -> 'r
  end

module Fmap4 (T : T4) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                       (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected

    val reify : (helper -> ('a, 'b) injected -> 'b) -> (helper -> ('c, 'd) injected -> 'd) ->
                (helper -> ('e, 'f) injected -> 'f) -> (helper -> ('g, 'h) injected -> 'h) ->
                helper -> (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t logic as 'r) injected -> 'r
  end

module Fmap5 (T : T5) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                       (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected

    val reify : (helper -> ('a, 'b) injected -> 'b) -> (helper -> ('c, 'd) injected -> 'd) -> (helper -> ('e, 'f) injected -> 'f) ->
                (helper -> ('g, 'h) injected -> 'h) -> (helper -> ('i, 'j) injected -> 'j) ->
                helper -> (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t logic as 'r) injected -> 'r
  end

module Fmap6 (T : T6) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                       (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected

    val reify : (helper -> ('a, 'b) injected -> 'b) -> (helper -> ('c, 'd) injected -> 'd) -> (helper -> ('e, 'f) injected -> 'f) ->
                (helper -> ('g, 'h) injected -> 'h) -> (helper -> ('i, 'j) injected -> 'j) -> (helper -> ('k, 'l) injected -> 'l) ->
                helper -> (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t logic as 'r) injected -> 'r
  end

(* A default shallow reifier *)
val reify : helper -> ('a, 'a logic) injected -> 'a logic
