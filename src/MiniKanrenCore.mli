(*
 * MiniKanren: miniKanren implementation.
 * Copyright (C) 2015-2016
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin,
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

(* module OldList : (module type of List) *)

val generic_show: ?maxdepth:int -> 'a -> string

val printfn : ('a, unit, string, unit) format4 -> 'a

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

(** {3 Lazy streams} *)
module MKStream :
  sig
    (** Stream type *)
    type t

    val inc: (unit -> t) -> t
    (* val inc2: (unit -> 'a -> 'b t) -> 'a -> 'b t
    val inc3: ('a -> unit -> 'b t) -> 'a -> 'b t *)
    val mplus : t -> t -> t
    (* val mplus_star : 'a t list -> 'a t *)

    val bind: t -> ('a ->  t) -> t
  end

module Stream :
  sig
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

(**  The type [('a, 'b) injected] describes an injection of a type ['a] into ['b] *)
type ('a, 'b) injected

(** A state *)
module State :
  sig
    (** State type *)
    type t

    (** Printing helper *)
    (* val show : t -> string *)

    val new_var : t -> ('a, 'b) injected * int
    val incr_scope : t -> t
  end

(** Goal converts a state into a lazy stream of states *)
type 'a goal'
type goal = MKStream.t goal'

(** {3 Logical values and injections} *)


(** A type of abstract logic values *)
@type 'a logic =
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

(** [lift x] injects [x] into itself *)
val lift : 'a -> ('a, 'a) injected

(** [inj x] injects [x] into logical [x] *)
val inj : ('a, 'b) injected -> ('a, 'b logic) injected

(** A synonym for [fun x -> inj @@ lift x] (for non-parametric types) *)
val (!!) : 'a -> ('a, 'a logic) injected

(** [prj x] returns plain value from injected representation.
    Raises exception [Not_a_value] if [x] contains logic variables. *)
val prj : ('a, 'b) injected -> 'a

(** [to_logic x] makes logic value from plain one *)
val to_logic : 'a -> 'a logic

(** [from_logic x] makes plain value from logic one.
    Raises exception [Not_a_value] if [x] contains logic variables. *)
val from_logic : 'a logic -> 'a

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'b) injected -> goal) -> goal

val report_counters : unit -> unit

(** [x === y] creates a goal, which performs a unification of [x] and [y] *)
val (===) : ?loc:string -> ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for [x] and [y] *)
val (=/=) : ('a, 'b logic) injected -> ('a, 'b logic) injected -> goal

(** [conj s1 s2] creates a goal, which is a conjunction of its arguments *)
val conj : goal -> goal -> goal

(** [&&&] is left-associative infix synonym for [conj] *)
val (&&&) : goal -> goal -> goal

(** [disj s1 s2] creates a goal, which is a disjunction of its arguments *)
val disj : goal -> goal -> goal

(** [|||] is left-associative infix synonym for [disj] *)
val (|||) : goal -> goal -> goal

(** [?| [s1; s2; ...; sk]] calculates [s1 ||| s2 ||| ... ||| sk] for a non-empty list of goals *)
val (?|) : goal list -> goal

(** [conde] is a synonym for [?|] *)
val conde : goal list -> goal

(** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a non-empty list of goals *)
val (?&) : goal list -> goal

val bind_star : goal list -> goal

(** {2 Some predefined goals} *)

(** [success] always succeeds *)
val success : goal

(** [failure] always fails *)
val failure : goal

(** {2 Combinators to produce fresh variables} *)
module Fresh :
  sig
    (** [succ num f] increments the number of free logic variables in
        a goal; can be used to get rid of ``fresh'' syntax extension *)
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

(** [run n g h] runs a goal [g] with [n] logical parameters and passes refined results to the handler [h]. The number of parameters is encoded using variadic
    machinery {a la} Danvy and represented by a number of predefined numerals and successor function (see below). The refinement replaces each variable, passed
    to [g], with the stream of values, associated with that variables as the goal succeeds.

    Examples:

    - [run one        (fun q   -> q === !!5)               (fun qs    -> ]{i here [q]s       --- a stream of all values, associated with the variable [q]}[)]
    - [run two        (fun q r -> q === !!5 ||| r === !!6) (fun qs rs -> ]{i here [qs], [rs] --- streams of all values, associated with the variable [q] and [r], respectively}[)]
    - [run (succ one) (fun q r -> q === !!5 ||| r === !!6) (fun qs rs -> ]{i the same as the above}[)]
 *)
val run : (unit -> ('a -> 'c goal') * ('d -> 'e -> 'f) *
                      (('g Stream.t -> 'h -> 'e) * ('c -> 'h * MKStream.t))) ->
          'a -> 'd -> 'f

(**
  The primitive [delay] helps to construct recursive goals that depend on themselves. For example,
  we can't write [let fives q = (q===!!5) ||| (fives q)] because generation of this goal leads to
  infinite reciursion. The right way to implement this is [let fives q = (q===!!5) ||| delay (fun () -> fives q)]

  See also syntax extension which simplifies the syntax.
*)
val delay  : (unit -> goal) -> goal

val trace: string -> goal -> goal

(** Reification helper *)
type helper

val project1: msg:string -> (helper -> ('a, 'b) injected -> string) -> ('a, 'b) injected -> goal
val project2: msg:string -> (helper -> ('a, 'b) injected -> string) -> ('a, 'b) injected -> ('a, 'b) injected -> goal
val project3: msg:string -> (helper -> ('a, 'b) injected -> string) ->
    ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) injected -> goal

(* Like (===) but with tracing *)
val unitrace: ?loc:string -> (helper -> ('a, 'b) injected -> string) -> ('a, 'b) injected -> ('a, 'b) injected -> goal

val diseqtrace: (helper -> ('a, 'b) injected -> string) -> ('a, 'b) injected -> ('a, 'b) injected -> goal

(**
  The exception is raised when we try to extract plain term from the answer but only terms with free
  variables are possible.
*)
exception Not_a_value

(** Reification result *)
class type ['a,'b] refined = object
  (** Returns [true] if the term has any free logic variable inside *)
  method is_open: bool

  (**
    Get the answer as plain term. Raises exception [Not_a_value] when only terms with free variables
    are available.
  *)
  method prj: 'a

  (**
    Get the answer as non-flat value. If the actual answer is a flat value it will be injected using
    the function provided.
   *)
  method refine: (helper -> ('a, 'b) injected -> 'b) -> inj:('a -> 'b) -> 'b
end

(** A type to refine a stream of states into the stream of answers (w.r.t. some known logic variable *)
type ('a, 'b) refiner

(** Successor function *)
val succ :
           (unit ->
            ('a -> 'b goal') * ('c -> 'd -> 'e) *
            ((State.t Stream.t -> 'f -> 'g) * ('h -> 'i * 'j))) ->
           unit ->
           ((('k, 'l) injected -> 'a) -> (('k, 'l) refiner * 'b) goal') *
           (('m -> 'c) -> 'm * 'd -> 'e) *
           ((State.t Stream.t ->
             ('n, 'o) refiner * 'f -> ('n, 'o) refined Stream.t * 'g) *
            ('p * 'h -> ('p * 'i) * 'j))

(** {3 Predefined numerals (one to five)} *)

val one : unit ->
  ((('a,'c) injected -> 'b goal') -> (('a, 'c) refiner * 'b) goal') *
  (('d -> 'e) -> 'd -> 'e) *
  ((State.t Stream.t ->
    ('f, 'g) refiner -> ('f, 'g) refined Stream.t) *
    ('h -> 'h))

val two : unit ->
  ((('a,'d) injected -> ('b,'e) injected -> 'c goal') ->
    (('a, 'd) refiner * (('b, 'e) refiner * 'c)) goal') *
    (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
    ((State.t Stream.t ->
      ('i, 'j) refiner * ('k, 'l) refiner ->
      ('i, 'j) refined Stream.t *
      ('k, 'l) refined Stream.t) *
     ('m * ('n * 'o) -> ('m * 'n) * 'o))

val three : unit ->
  (( ('a,'e) injected -> ('b, 'f) injected -> ('c, 'g) injected -> 'd goal') ->
    (('a, 'e) refiner * (('b, 'f) refiner * (('c, 'g) refiner * 'd))) goal') *
   (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
   ((State.t Stream.t ->
     ('l, 'm) refiner * (('n, 'o) refiner * ('p, 'q) refiner) ->
     ('l, 'm) refined Stream.t *
     (('n, 'o) refined Stream.t *
      ('p, 'q) refined Stream.t)) *
    ('r * ('s * ('t * 'u)) -> ('r * ('s * 't)) * 'u))

val four : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> 'f goal') ->
    (('a, 'g) refiner * (('b, 'h) refiner * (('c, 'i) refiner * (('d, 'j) refiner * 'f)))) goal') *
  (('l -> 'm -> 'n -> 'o -> 'q) ->
    'l * ('m * ('n * 'o)) -> 'q) *
   ((State.t Stream.t ->
     ('r, 's) refiner * (('t, 'u) refiner * (('v, 'w) refiner * ('x, 'y) refiner)) ->
     ('r, 's) refined Stream.t *
     (('t, 'u) refined Stream.t *
      (('v, 'w) refined Stream.t *
       (('x, 'y) refined Stream.t)))) *
    ('b1 * ('c1 * ('d1 * ('e1 * 'f1))) ->
     ('b1 * ('c1 * ('d1 * 'e1)))  * 'f1))

val five : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> ('e, 'k) injected -> 'f goal') ->
   (('a, 'g) refiner *
    (('b, 'h) refiner *
     (('c, 'i) refiner * (('d, 'j) refiner * (('e, 'k) refiner * 'f))))) goal') *
  (('l -> 'm -> 'n -> 'o -> 'p -> 'q) ->
   'l * ('m * ('n * ('o * 'p))) -> 'q) *
   ((State.t Stream.t ->
     ('r, 's) refiner *
     (('t, 'u) refiner *
      (('v, 'w) refiner * (('x, 'y) refiner * ('z, 'a1) refiner))) ->
     ('r, 's) refined Stream.t *
     (('t, 'u) refined Stream.t *
      (('v, 'w) refined Stream.t *
       (('x, 'y) refined Stream.t *
        ('z, 'a1) refined Stream.t)))) *
    ('b1 * ('c1 * ('d1 * ('e1 * ('f1 * 'g1)))) ->
     ('b1 * ('c1 * ('d1 * ('e1 * 'f1)))) * 'g1))

(** {3 The same numerals with conventional names} *)
val q : unit ->
  ((('a,'c) injected -> 'b goal') -> (('a, 'c) refiner * 'b) goal') *
  (('d -> 'e) -> 'd -> 'e) *
  ((State.t Stream.t ->
    ('f, 'g) refiner -> ('f, 'g) refined Stream.t) *
    ('h -> 'h))

val qr : unit ->
  ((('a,'d) injected -> ('b,'e) injected -> 'c goal') ->
    (('a, 'd) refiner * (('b, 'e) refiner * 'c)) goal') *
    (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
    ((State.t Stream.t ->
      ('i, 'j) refiner * ('k, 'l) refiner ->
      ('i, 'j) refined Stream.t *
      ('k, 'l) refined Stream.t) *
     ('m * ('n * 'o) -> ('m * 'n) * 'o))

val qrs : unit ->
  (( ('a,'e) injected -> ('b, 'f) injected -> ('c, 'g) injected -> 'd goal') ->
    (('a, 'e) refiner * (('b, 'f) refiner * (('c, 'g) refiner * 'd))) goal') *
   (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
   ((State.t Stream.t ->
     ('l, 'm) refiner * (('n, 'o) refiner * ('p, 'q) refiner) ->
     ('l, 'm) refined Stream.t *
     (('n, 'o) refined Stream.t *
      ('p, 'q) refined Stream.t)) *
    ('r * ('s * ('t * 'u)) -> ('r * ('s * 't)) * 'u))

val qrst : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> 'f goal') ->
    (('a, 'g) refiner * (('b, 'h) refiner * (('c, 'i) refiner * (('d, 'j) refiner * 'f)))) goal') *
  (('l -> 'm -> 'n -> 'o -> 'q) ->
    'l * ('m * ('n * 'o)) -> 'q) *
   ((State.t Stream.t ->
     ('r, 's) refiner * (('t, 'u) refiner * (('v, 'w) refiner * ('x, 'y) refiner)) ->
     ('r, 's) refined Stream.t *
     (('t, 'u) refined Stream.t *
      (('v, 'w) refined Stream.t *
       (('x, 'y) refined Stream.t)))) *
    ('b1 * ('c1 * ('d1 * ('e1 * 'f1))) ->
     ('b1 * ('c1 * ('d1 * 'e1)))  * 'f1))

(** {2 Building reifiers compositionally } *)
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

module Fmap1 (T : T1) :
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

val simple_reifier: helper -> ('a, 'a logic) injected -> 'a logic
