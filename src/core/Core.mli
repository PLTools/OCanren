(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2025
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
    (** @canonical OCanren.State.t *)
    type t
  end

(** Goal is a function that converts a state into a lazy stream of states. *)
type 'a goal'

(** @canonical OCanren.goal *)
type goal = State.t Stream.t goal'

(** {3 miniKanren basic combinators} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter. See also {!module-Fresh} to create variables in numbers. *)
val call_fresh : ('a ilogic -> goal) -> goal

val wc : ('a ilogic -> goal) -> goal

(** [x === y] creates a goal, which performs a unification of [x] and [y] *)
val (===) : 'a ilogic -> 'a ilogic -> goal

(** [unify x y] is a prefix synonym for [x === y] *)
val unify : 'a ilogic -> 'a ilogic -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for [x] and [y] *)
val (=/=) : 'a ilogic -> 'a ilogic -> goal

(** [diseq x y] is a prefix synonym for [x =/= y] *)
val diseq : 'a ilogic -> 'a ilogic -> goal

(** Call [structural var reifier checker] adds a structural constraint for future use.
 Every time substitution is updated it reifies [var] using [reifier] and checks that
  the result satisfies desired predicate [checker].

 The predicate [checker] returns false when constraint is violated.
 Call [structural] saves constraint and return substitution nonmodified.

 See also: {!debug_var}.
*)
val structural :
  'a  ->
  ('a, 'b) Reifier.t ->
  ('b -> bool) ->
  goal


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

val condo2 : goal -> goal -> goal

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
    val succ : ('a -> 'b goal') -> (_ ilogic -> 'a) -> 'b goal'

    (** Zero logic parameters *)
    val zero : 'a -> 'a

    (** {3 One to five logic parameter(s)} *)
    val one   : (_ ilogic ->                                                         goal) -> goal
    val two   : (_ ilogic -> _ ilogic ->                                           goal) -> goal
    val three : (_ ilogic -> _ ilogic -> _ ilogic ->                             goal) -> goal
    val four  : (_ ilogic -> _ ilogic -> _ ilogic -> _ ilogic ->               goal) -> goal
    val five  : (_ ilogic -> _ ilogic -> _ ilogic -> _ ilogic -> _ ilogic -> goal) -> goal

    (** {3 One to five logic parameter(s), conventional names} *)
    val q     : (_ ilogic ->                                                         goal) -> goal
    val qr    : (_ ilogic -> _ ilogic ->                                           goal) -> goal
    val qrs   : (_ ilogic -> _ ilogic -> _ ilogic ->                             goal) -> goal
    val qrst  : (_ ilogic -> _ ilogic -> _ ilogic -> _ ilogic ->               goal) -> goal
    val pqrst : (_ ilogic -> _ ilogic -> _ ilogic -> _ ilogic -> _ ilogic -> goal) -> goal
  end

(** {2 Top-level running primitives} *)

(** The primitive [delay] helps to construct recursive goals, which depend on themselves. For example,
    we can't write [let rec fives q = (q === !!5) ||| (fives q)] because the generation of this goal leads to
    infinite recursion. The correct way to implement this is [let rec fives q = (q === !!5) ||| delay (fun () -> fives q)]

    See also syntax extension [defer].
*)
val delay : (unit -> goal) -> goal

(** [run n g h] runs a goal [g] with [n] logical parameters and passes reified results to the handler [h].
    The number of parameters is encoded using variadic machinery {i à la} Olivier Danvy and represented by
    a number of predefined numerals and successor function (see below). The reification replaces each variable,
    passed to [g], with the stream of values, associated with that variable as the goal succeeds.

    See also original Olivier's Danvy
    {{: https://www.brics.dk/RS/98/12/BRICS-RS-98-12.pdf}paper}
    ``Functional Unparsing''.

    Examples:

    - [run one        (fun q   -> q === !!5)               (fun qs    -> ...)]. Here [qs] --- a stream of all values, associated with the variable [q].
    - [run two        (fun q r -> q === !!5 ||| r === !!6) (fun qs rs -> ...)]. Here [qs], [rs] --- streams of all values, associated with the variable [q] and [r], respectively.
    - [run (succ one) (fun q r -> q === !!5 ||| r === !!6) (fun qs rs -> ...)]. The same as the above.
*)
val run : (unit ->
            ('a -> State.t -> 'b) * ('c -> Env.t -> 'd) *
            ('b -> 'c * State.t Stream.t) * ('e -> 'd -> 'f)) ->
           'a -> 'e -> 'f Stream.t

(** Successor function *)
val succ : (unit ->
            ('a -> State.t -> 'b) * ('c -> Env.t -> 'd) * ('e -> 'f * 'g) *
            ('h -> 'i -> 'j)) ->
           unit ->
           (('k ilogic -> 'a) -> State.t -> 'k ilogic * 'b) *
           ('m ilogic * 'c -> Env.t -> 'm reified * 'd) *
           ('o * 'e -> ('o * 'f) * 'g) * (('p -> 'h) -> 'p * 'i -> 'j)

(** A module with predefined type aliases for numerals [one], [succ one], etc. *)
module NUMERAL_TYPS : sig
  type ('a, 'c, 'e, 'f, 'g) one = unit ->
           (('a ilogic -> goal) ->
            State.t -> 'a ilogic * State.t Stream.t) *
           ('c ilogic -> Env.t -> 'c reified) * ('e -> 'e) *
           (('f -> 'g) -> 'f -> 'g)
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) two = unit ->
           (('a Logic.ilogic -> 'b Logic.ilogic -> goal) ->
            State.t -> 'a Logic.ilogic * ('b Logic.ilogic * State.t Stream.t)) *
           ('c Logic.ilogic * 'd Logic.ilogic ->
            Env.t -> 'c Logic.reified * 'd Logic.reified) *
           ('e * ('f * 'g) -> ('e * 'f) * 'g) *
           (('h -> 'i -> 'j) -> 'h * 'i -> 'j)

  type ('a,'c,'e,'g,'i,'k,'m,'n,'o,'p,'q,'r,'s,'t) three = unit ->
           (('a ilogic -> 'c ilogic -> 'e ilogic -> goal) ->
            State.t ->
            'a ilogic *
            ('c ilogic *
             ('e ilogic * State.t Stream.t))) *
           ('g ilogic * ('i ilogic * 'k ilogic) ->
            Env.t ->
            'g reified * ('i reified * 'k reified)) *
           ('m * ('n * ('o * 'p)) -> ('m * ('n * 'o)) * 'p) *
           (('q -> 'r -> 's -> 't) -> 'q * ('r * 's) -> 't)

  type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r) four = unit ->
         (('a ilogic -> 'b ilogic -> 'c ilogic -> 'd ilogic -> goal) ->
          State.t ->
          'a ilogic *
          ('b ilogic * ('c ilogic * ('d ilogic * State.t Stream.t)))) *
         ('e ilogic * ('f ilogic * ('g ilogic * 'h ilogic)) ->
          Env.t -> 'e reified * ('f reified * ('g reified * 'h reified))) *
         ('i * ('j * ('k * ('l * 'm))) -> ('i * ('j * ('k * 'l))) * 'm) *
         (('n -> 'o -> 'p -> 'q -> 'r) -> 'n * ('o * ('p * 'q)) -> 'r)

end


(** {3 Predefined numerals (one to five)} *)
val one : (_, _, _, _, _) NUMERAL_TYPS.one
val two : (_, _, _, _, _, _, _, _, _, _) NUMERAL_TYPS.two
val three : (_, _, _, _, _, _, _, _, _, _, _, _, _, _) NUMERAL_TYPS.three
val four : (_, _, _, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_) NUMERAL_TYPS.four


(** {3 The same numerals with conventional names} *)
val q : (_, _, _,  _, _) NUMERAL_TYPS.one
val qr : (_, _, _, _, _, _, _, _, _, _) NUMERAL_TYPS.two
val qrs : (_, _, _, _, _, _, _, _, _, _, _, _, _, _) NUMERAL_TYPS.three
val qrst : (_, _, _, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_) NUMERAL_TYPS.four


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
           (('g ilogic * 'a -> 'b) -> 'g ilogic -> 'c) *
           (('i -> 'd) -> 'i * 'e -> 'f)

    val one : unit ->
         (('a ilogic -> 'c) -> 'a ilogic -> 'c) *
         (('d -> 'e) -> 'd -> 'e)

    val two : unit ->
         (('a ilogic * 'c ilogic -> 'e) ->
          'a ilogic -> 'c ilogic -> 'e) *
         (('f -> 'g -> 'h) -> 'f * 'g -> 'h)

    val three : unit ->
           (('a ilogic * ('c ilogic * 'e ilogic) ->
             'g) ->
            'a ilogic -> 'c ilogic -> 'e ilogic -> 'g) *
           (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k)

    val four :  unit ->
           (('a ilogic *
             ('c ilogic * ('e ilogic * 'g ilogic)) ->
             'i) ->
            'a ilogic ->
            'c ilogic -> 'e ilogic -> 'g ilogic -> 'i) *
           (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n)

    val five : unit ->
           (('a ilogic *
             ('c ilogic *
              ('e ilogic * ('g ilogic * 'i ilogic))) ->
             'k) ->
            'a ilogic ->
            'c ilogic ->
            'e ilogic -> 'g ilogic -> 'i ilogic -> 'k) *
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

IFDEF STATS THEN
val unification_counter : unit -> int
val unification_time    : unit -> Timer.span
val conj_counter        : unit -> int
val disj_counter        : unit -> int
val delay_counter       : unit -> int
END

(** The call [debug_var var reifier callback] performs reification of variable [var] in a current state using [reifier] and passes list of answer to [callback] (multiple answers can arise in presence of disequality constraints). The [callback] can investigate reified value and construct required goal to continue search.

See also: {!structural}.
*)
val debug_var : 'a ilogic -> (Env.t -> 'a ilogic -> 'b) -> ('b list -> goal) -> goal

(** The goal [only_head f] returns no answers when [f] returns:
  - empty stream when [f] returns empty stream;
  - hangs when [f] hangs during search for first answer;
  - stream with head answer if [f] returns stream that has at least 1 answer.
*)
val only_head : goal -> goal

module PrunesControl : sig
  val reset : unit -> unit
  val enable_skips: on:bool -> unit
  val set_max_skips: int -> unit
  val incr : unit -> unit
  val is_exceeded: unit -> bool
  val skipped_prunes : unit -> int
end

(** Runs reifier on empty state. Useful to debug execution order *)
val reify_in_empty: ('a, 'b) Reifier.t -> 'a -> 'b

val trace_diseq: goal
