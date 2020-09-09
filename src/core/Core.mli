(*
 * OCanren.
 * Copyright (C) 2015-2020
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

open Logic

module State :
  sig
    type t
  end

(** Goal converts a state into a lazy stream of states *)
type 'a goal'
type goal = State.t Stream.t goal'

(** {3 miniKanren basic combinators} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'b) injected -> goal) -> goal

(** [x === y] creates a goal, which performs a unification of [x] and [y] *)
val (===) : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(** [unify x y] is a prefix synonym for [x === y] *)
val unify : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for [x] and [y] *)
val (=/=) : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(* Call [structural var reifier checker] adds a structural constraint for future use.
 * Every time substitution is updated it reifies [var] using [reifier] and checks that
 * the result satisfies desired predicate [checker].
 *
 * The predicate [checker] returns false when constraint is violated.
 * Call [structural] saves constraint and return substitution nonmodified.
 * *)
val structural :
  ('a,'b) injected ->
  (Env.t -> ('a,'b) injected -> 'b) ->
  ('b -> bool) ->
  goal

(** [diseq x y] is a prefix synonym for [x =/= y] *)
val diseq : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

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


(** Identity (can be used in [run] to return the stream unchanged) *)
(* val id : 'a -> 'a *)

IFDEF STATS THEN
(** Unification counter *)
val unification_counter : unit -> int
val unification_time    : unit -> Mtime.span
val conj_counter        : unit -> int
val disj_counter        : unit -> int
val delay_counter       : unit -> int
END

val debug_var : ('a, 'b) injected -> (('a,'b) injected -> Env.t -> 'b) -> ('b list -> goal) -> goal

val only_head : goal -> goal

module PrunesControl : sig
  val reset : unit -> unit
  val enable_skips: on:bool -> unit
  val set_max_skips: int -> unit
  val incr : unit -> unit
  val is_exceeded: unit -> bool
  val skipped_prunes : unit -> int
end
