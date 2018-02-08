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
    (** Stream type *)
    type 'a t

    (** Constructors *)

    val nil : 'a t

    val single : 'a -> 'a t

    val cons : 'a -> 'a t -> 'a t

    val from_fun : (unit -> 'a t) -> 'a t

    val of_list : 'a list -> 'a t

    (** Emptiness test *)
    val is_empty : 'a t -> bool

    (** [map f s] maps function [f] over the stream [s] *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** [iter f s] iterates function [f] over the stream [s] *)
    val iter : ('a -> unit) -> 'a t -> unit

    (** [filter p g] filters the stream [g] using the predicate [p] (leaves only those elements [x], for which [p x = true]) *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

    (** [zip f g] returns a strem of corresponding pairs of [f] and [g]; fails for the streams of different lengths *)
    val zip : 'a t -> 'b t -> ('a * 'b) t

    (** [retrieve ~n:n s] returns the list of [n]-first elements of [s] and the rest of the stream *)
    val retrieve : ?n:int -> 'a t -> 'a list * 'a t

    (** [take ~n:n s] returns the list of [n]-first elements of [s] *)
    val take : ?n:int -> 'a t -> 'a list

    (** [hd s] gets a head of the stream *)
    val hd : 'a t -> 'a

    (** [hd s] gets a tail of the stream *)
    val tl : 'a t -> 'a t
  end

(** {3 States and goals} *)

module Env :
  sig
    type t
  end

(** A state *)
module State :
  sig
    (** State type *)
    type t
  end

(** Goal converts a state into a lazy stream of states *)
type 'a goal'
type goal = State.t Stream.t goal'

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
val run : (unit ->
            ('a -> State.t -> 'b) * ('c -> Env.t -> 'd) *
            ('b -> 'c * State.t Stream.t) * ('e -> 'd -> 'f)) ->
           'a -> 'e -> 'f Stream.t

(** The primitive [delay] helps to construct recursive goals, which depend on themselves. For example,
    we can't write [let rec fives q = (q === !!5) ||| (fives q)] because the generation of this goal leads to
    infinite recursion. The correct way to implement this is [let rec fives q = (q === !!5) ||| delay (fun () -> fives q)]

    See also syntax extension [defer].
*)
val delay : (unit -> goal) -> goal

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
  method reify: (Env.t -> ('a, 'b) injected -> 'b) -> 'b

  method prjc : (Env.t -> ('a, 'b) injected -> 'a) -> 'a
end

(** Successor function *)
val succ : (unit ->
            ('a -> State.t -> 'b) * ('c -> Env.t -> 'd) * ('e -> 'f * 'g) *
            ('h -> 'i -> 'j)) ->
           unit ->
           ((('k, 'l) injected -> 'a) -> State.t -> ('k, 'l) injected * 'b) *
           (('m, 'n) injected * 'c -> Env.t -> ('m, 'n) reified * 'd) *
           ('o * 'e -> ('o * 'f) * 'g) * (('p -> 'h) -> 'p * 'i -> 'j)

(** {3 Predefined numerals (one to five)} *)
val one : unit ->
           ((('a, 'b) injected -> goal) ->
            State.t -> ('a, 'b) injected * State.t Stream.t) *
           (('c, 'd) injected -> Env.t -> ('c, 'd) reified) * ('e -> 'e) *
           (('f -> 'g) -> 'f -> 'g)

val two : unit ->
           ((('a, 'b) injected -> ('c, 'd) injected -> goal) ->
            State.t ->
            ('a, 'b) injected * (('c, 'd) injected * State.t Stream.t)) *
           (('e, 'f) injected * ('g, 'h) injected ->
            Env.t -> ('e, 'f) reified * ('g, 'h) reified) *
           ('i * ('j * 'k) -> ('i * 'j) * 'k) *
           (('l -> 'm -> 'n) -> 'l * 'm -> 'n)

val three : unit ->
           ((('a, 'b) injected ->
             ('c, 'd) injected -> ('e, 'f) injected -> goal) ->
            State.t ->
            ('a, 'b) injected *
            (('c, 'd) injected *
             (('e, 'f) injected * State.t Stream.t))) *
           (('g, 'h) injected * (('i, 'j) injected * ('k, 'l) injected) ->
            Env.t ->
            ('g, 'h) reified * (('i, 'j) reified * ('k, 'l) reified)) *
           ('m * ('n * ('o * 'p)) -> ('m * ('n * 'o)) * 'p) *
           (('q -> 'r -> 's -> 't) -> 'q * ('r * 's) -> 't)

val four : unit ->
           ((('a, 'b) injected ->
             ('c, 'd) injected ->
             ('e, 'f) injected -> ('g, 'h) injected -> goal) ->
            State.t ->
            ('a, 'b) injected *
            (('c, 'd) injected *
             (('e, 'f) injected *
              (('g, 'h) injected * State.t Stream.t)))) *
           (('i, 'j) injected *
            (('k, 'l) injected * (('m, 'n) injected * ('o, 'p) injected)) ->
            Env.t ->
            ('i, 'j) reified *
            (('k, 'l) reified * (('m, 'n) reified * ('o, 'p) reified))) *
           ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u) *
           (('v -> 'w -> 'x -> 'y -> 'z) -> 'v * ('w * ('x * 'y)) -> 'z)

val five : unit ->
           ((('a, 'b) injected ->
             ('c, 'd) injected ->
             ('e, 'f) injected ->
             ('g, 'h) injected -> ('i, 'j) injected -> goal) ->
            State.t ->
            ('a, 'b) injected *
            (('c, 'd) injected *
             (('e, 'f) injected *
              (('g, 'h) injected *
               (('i, 'j) injected * State.t Stream.t))))) *
           (('k, 'l) injected *
            (('m, 'n) injected *
             (('o, 'p) injected * (('q, 'r) injected * ('s, 't) injected))) ->
            Env.t ->
            ('k, 'l) reified *
            (('m, 'n) reified *
             (('o, 'p) reified * (('q, 'r) reified * ('s, 't) reified)))) *
           ('u * ('v * ('w * ('x * ('y * 'z)))) ->
            ('u * ('v * ('w * ('x * 'y)))) * 'z) *
           (('a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1 -> 'f1) ->
            'a1 * ('b1 * ('c1 * ('d1 * 'e1))) -> 'f1)

(** {3 The same numerals with conventional names} *)
val q : unit ->
           ((('a, 'b) injected -> goal) ->
            State.t -> ('a, 'b) injected * State.t Stream.t) *
           (('c, 'd) injected -> Env.t -> ('c, 'd) reified) * ('e -> 'e) *
           (('f -> 'g) -> 'f -> 'g)

val qr : unit ->
           ((('a, 'b) injected -> ('c, 'd) injected -> goal) ->
            State.t ->
            ('a, 'b) injected * (('c, 'd) injected * State.t Stream.t)) *
           (('e, 'f) injected * ('g, 'h) injected ->
            Env.t -> ('e, 'f) reified * ('g, 'h) reified) *
           ('i * ('j * 'k) -> ('i * 'j) * 'k) *
           (('l -> 'm -> 'n) -> 'l * 'm -> 'n)

val qrs : unit ->
           ((('a, 'b) injected ->
             ('c, 'd) injected -> ('e, 'f) injected -> goal) ->
            State.t ->
            ('a, 'b) injected *
            (('c, 'd) injected *
             (('e, 'f) injected * State.t Stream.t))) *
           (('g, 'h) injected * (('i, 'j) injected * ('k, 'l) injected) ->
            Env.t ->
            ('g, 'h) reified * (('i, 'j) reified * ('k, 'l) reified)) *
           ('m * ('n * ('o * 'p)) -> ('m * ('n * 'o)) * 'p) *
           (('q -> 'r -> 's -> 't) -> 'q * ('r * 's) -> 't)

val qrst : unit ->
           ((('a, 'b) injected ->
             ('c, 'd) injected ->
             ('e, 'f) injected -> ('g, 'h) injected -> goal) ->
            State.t ->
            ('a, 'b) injected *
            (('c, 'd) injected *
             (('e, 'f) injected *
              (('g, 'h) injected * State.t Stream.t)))) *
           (('i, 'j) injected *
            (('k, 'l) injected * (('m, 'n) injected * ('o, 'p) injected)) ->
            Env.t ->
            ('i, 'j) reified *
            (('k, 'l) reified * (('m, 'n) reified * ('o, 'p) reified))) *
           ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u) *
           (('v -> 'w -> 'x -> 'y -> 'z) -> 'v * ('w * ('x * 'y)) -> 'z)

val qrstu : unit ->
           ((('a, 'b) injected ->
             ('c, 'd) injected ->
             ('e, 'f) injected ->
             ('g, 'h) injected -> ('i, 'j) injected -> goal) ->
            State.t ->
            ('a, 'b) injected *
            (('c, 'd) injected *
             (('e, 'f) injected *
              (('g, 'h) injected *
               (('i, 'j) injected * State.t Stream.t))))) *
           (('k, 'l) injected *
            (('m, 'n) injected *
             (('o, 'p) injected * (('q, 'r) injected * ('s, 't) injected))) ->
            Env.t ->
            ('k, 'l) reified *
            (('m, 'n) reified *
             (('o, 'p) reified * (('q, 'r) reified * ('s, 't) reified)))) *
           ('u * ('v * ('w * ('x * ('y * 'z)))) ->
            ('u * ('v * ('w * ('x * 'y)))) * 'z) *
           (('a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1 -> 'f1) ->
            'a1 * ('b1 * ('c1 * ('d1 * 'e1))) -> 'f1)

 (** Tabling primitives.
     Tabling allows to cache answers of the goal between different queries.

    Usage:
      General form : [Tabling.tabled/tabledrec n g] where [n] is the number of parameters and [g] is the goal.
      Returns modified `tabled` goal.

      1) For non-recursive goals:

        [let g = Tabling.(tabled two) (fun q r -> q === r)]

      2) For recursive goals:
        In this case it is necessery to `abstract` from recursive calls.
        The goal should take additional (first) argument [grec] and use it instead of recursive calls to itself.

        [let g = Tabling.(tabledrec one) (fun grec q -> (q === O) ||| (fresh (n) (q === S n) &&& (grec n)))]
 *)

module Tabling :
  sig
    val succ : (unit -> (('a -> 'b) -> 'c) * ('d -> 'e -> 'f)) ->
           unit ->
           ((('g, 'h) injected * 'a -> 'b) -> ('g, 'h) injected -> 'c) *
           (('i -> 'd) -> 'i * 'e -> 'f)

    val one : unit ->
         ((('a, 'b) injected -> 'c) -> ('a, 'b) injected -> 'c) *
         (('d -> 'e) -> 'd -> 'e)

    val two : unit ->
         ((('a, 'b) injected * ('c, 'd) injected -> 'e) ->
          ('a, 'b) injected -> ('c, 'd) injected -> 'e) *
         (('f -> 'g -> 'h) -> 'f * 'g -> 'h)

    val three : unit ->
           ((('a, 'b) injected * (('c, 'd) injected * ('e, 'f) injected) ->
             'g) ->
            ('a, 'b) injected -> ('c, 'd) injected -> ('e, 'f) injected -> 'g) *
           (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k)

    val four :  unit ->
           ((('a, 'b) injected *
             (('c, 'd) injected * (('e, 'f) injected * ('g, 'h) injected)) ->
             'i) ->
            ('a, 'b) injected ->
            ('c, 'd) injected -> ('e, 'f) injected -> ('g, 'h) injected -> 'i) *
           (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n)

    val five : unit ->
           ((('a, 'b) injected *
             (('c, 'd) injected *
              (('e, 'f) injected * (('g, 'h) injected * ('i, 'j) injected))) ->
             'k) ->
            ('a, 'b) injected ->
            ('c, 'd) injected ->
            ('e, 'f) injected -> ('g, 'h) injected -> ('i, 'j) injected -> 'k) *
           (('l -> 'm -> 'n -> 'o -> 'p -> 'q) ->
            'l * ('m * ('n * ('o * 'p))) -> 'q)

    val tabled : (unit ->
            (('a -> State.t Stream.t goal') -> 'b) *
            ('c -> 'a -> State.t Stream.t goal')) ->
           'c -> 'b

    val tabledrec : (unit ->
       (('a -> State.t Stream.t goal') -> 'b -> 'c) *
       ('d -> 'a -> State.t Stream.t goal')) ->
      (('b -> 'c) -> 'd) -> 'b -> 'c
  end


(** {2 Building reifiers for a custom type compositionally} *)
include module type of struct include MiniKanrenTypes.Ts end

include MiniKanrenTypes.Fmaps with type ('a,'b) inj = ('a,'b) injected
                               and type helper = Env.t
                               and type 'a logic_ = 'a logic

(* A default shallow reifier *)
val reify : Env.t -> ('a, 'a logic) injected -> 'a logic

val prjc : (int -> 'a list -> 'a) -> Env.t -> ('a, 'a logic) injected -> 'a
