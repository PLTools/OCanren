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

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

(** {3 Lazy streams} *)
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
  end

(** {3 States and goals} *)

(** A state *)
module State :
  sig
    (** State type *)
    type t

    (** Printing helper *)
    val show : t -> string
  end

(** Goal converts a state into a lazy stream of states *)
type goal = State.t -> State.t Stream.t

(** {3 Logical values and injections} *)

(**  The type [('a, 'b) injected] describes an injection of a type ['a] into ['b] *)
type ('a, 'b) injected

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

(** A synonym for [inj @@ lift] (for non-parametric types) *)
val (!!) : 'a -> ('a, 'a logic) injected

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'b) injected -> State.t -> 'r) -> State.t -> 'r

(** [x === y] creates a goal, which performs a unifications of [x] and [y] *)
val (===) : ('a, 'b) injected -> ('a, 'b) injected -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for [x] and [y] *)
val (=/=) : ('a, 'b) injected -> ('a, 'b) injected -> goal

(** [conj s1 s2] creates a goal, which is a conjunction of its arguments *)
val conj : goal -> goal -> goal

(** [&&&] is left-associative infix synonym for [conj] *)
val (&&&) : goal -> goal -> goal

(** [disj s1 s2] creates a goal, which is a disjunction of its arguments *)
val disj : goal -> goal -> goal

(** [|||] is left-associative infix synonym for [disj] *)
val (|||) : goal -> goal -> goal

(** [?| [s1; s2; ...; sk]] calculates [s1 ||| s2 ||| ... ||| sk] for a
    non-empty list of goals. Calculations are performed from right to left. *)
val (?|) : goal list -> goal

(** [conde] is a synonym for [?|] *)
val conde : goal list -> goal

(** [conder] is a synonym for [?|] and [conde] *)
val conder : goal list -> goal

(** [condel] is the same as (?|) but calculation from left to right *)
val condel : goal list -> goal

(** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a
    non-empty list of goals; evaluation from left to right *)
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
        a goal; can be used to get rid of ``fresh'' syntax extension *)
    val succ : ('a -> State.t -> 'b) -> ((_, _) injected -> 'a) -> State.t -> 'b

    (** Zero logic parameters *)
    val zero : 'a -> 'a

    (** {3 One to five logic parameter(s)} *)
    val one   : (_ injected ->                                                         State.t -> 'r) -> State.t -> 'r
    val two   : (_ injected -> _ injected ->                                           State.t -> 'r) -> State.t -> 'r
    val three : (_ injected -> _ injected -> _ injected ->                             State.t -> 'r) -> State.t -> 'r
    val four  : (_ injected -> _ injected -> _ injected -> _ injected ->               State.t -> 'r) -> State.t -> 'r
    val five  : (_ injected -> _ injected -> _ injected -> _ injected -> _ injected -> State.t -> 'r) -> State.t -> 'r

    (** {3 One to five logic parameter(s), conventional names} *)
    val q     : (_ injected ->                                                         State.t -> 'r) -> State.t -> 'r
    val qr    : (_ injected -> _ injected ->                                           State.t -> 'r) -> State.t -> 'r
    val qrs   : (_ injected -> _ injected -> _ injected ->                             State.t -> 'r) -> State.t -> 'r
    val qrst  : (_ injected -> _ injected -> _ injected -> _ injected ->               State.t -> 'r) -> State.t -> 'r
    val pqrst : (_ injected -> _ injected -> _ injected -> _ injected -> _ injected -> State.t -> 'r) -> State.t -> 'r
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
val run : (unit -> ('a -> State.t -> 'c) * ('d -> 'e -> 'f) * (('g -> 'h -> 'e) * ('c -> 'h * 'g))) -> 'a -> 'd -> 'f

(** Reification helper *)
type helper

(** Reification result *)
type ('a, 'b) reification_rez =
| Final       of 'a
| HasFreeVars of ((helper -> ('a, 'b) injected -> 'b) -> 'b)

(** A type to refine a stream of states into the stream of answers (w.r.t. some known logic variable *)
type ('a, 'b) refiner = State.t Stream.t -> ('a, 'b) reification_rez Stream.t

(** Successor function *)
val succ :
    (unit ->
     ('a -> State.t -> 'b) * ('c -> 'd -> 'e) *
     (('f -> 'g -> 'h) * ('i -> 'j * 'k))) ->
    unit ->
    (('l -> 'a) -> State.t -> ('l, 'm) refiner * 'b) *
    (('n -> 'c) -> 'n * 'd -> 'e) *
    (('f -> ('f -> 'o) * 'g -> 'o * 'h) * ('p * 'i -> ('p * 'j) * 'k))

(** {3 Predefined numerals (one to five)} *)
val one : unit ->
  ((('a,'c) injected -> State.t -> 'b) -> State.t -> ('a, 'c) refiner * 'b) *
  (('d -> 'e) -> 'd -> 'e) * (('f -> ('f -> 'g) -> 'g) * ('h -> 'h))

val two : unit ->
  ((('a, 'd) injected -> ('b, 'e) injected -> State.t -> 'c) ->
   State.t -> ('a, 'd) refiner * (('b, 'e) refiner * 'c)) *
  (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
  (('i -> ('i -> 'j) * ('i -> 'k) -> 'j * 'k) *
   ('l * ('m * 'n) -> ('l * 'm) * 'n))

val three : unit ->
  (( ('a,'e) injected -> ('b, 'f) injected -> ('c, 'g) injected -> State.t -> 'd) ->
   State.t ->
   ('a, 'e) refiner * (('b, 'f) refiner * (('c, 'g) refiner * 'd))) *
  (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * ('l -> 'o)) -> 'm * ('n * 'o)) *
   ('p * ('q * ('r * 's)) -> ('p * ('q * 'r)) * 's))

val four : unit ->
  ((('a, 'f) injected -> ('b, 'g) injected -> ('c, 'h) injected -> ('d, 'i) injected -> State.t -> 'e) ->
   State.t ->
   ('a, 'f) refiner *
   (('b, 'g) refiner * (('c, 'h) refiner * (('d, 'i) refiner * 'e)))) *
  (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n) *
  (('o ->
    ('o -> 'p) * (('o -> 'q) * (('o -> 'r) * ('o -> 's))) ->
    'p * ('q * ('r * 's))) *
   ('t * ('u * ('v * ('w * 'x))) -> ('t * ('u * ('v * 'w))) * 'x))

val five : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> ('e, 'k) injected -> State.t -> 'f) ->
   State.t ->
   ('a, 'g) refiner *
   (('b, 'h) refiner *
    (('c, 'i) refiner * (('d, 'j) refiner * (('e, 'k) refiner * 'f))))) *
  (('l -> 'm -> 'n -> 'o -> 'p -> 'q) ->
   'l * ('m * ('n * ('o * 'p))) -> 'q) *
  (('r ->
    ('r -> 's) *
    (('r -> 't) * (('r -> 'u) * (('r -> 'v) * ('r -> 'w)))) ->
    's * ('t * ('u * ('v * 'w)))) *
   ('x * ('y * ('z * ('a1 * ('b1 * 'c1)))) ->
    ('x * ('y * ('z * ('a1 * 'b1)))) * 'c1))

(** {3 The same numerals with conventional names} *)
val q : unit ->
  ((('a,'c) injected -> State.t -> 'b) -> State.t -> ('a, 'c) refiner * 'b) *
  (('d -> 'e) -> 'd -> 'e) * (('f -> ('f -> 'g) -> 'g) * ('h -> 'h))

val qr : unit ->
  ((('a, 'd) injected -> ('b, 'e) injected -> State.t -> 'c) ->
   State.t -> ('a, 'd) refiner * (('b, 'e) refiner * 'c)) *
  (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
  (('i -> ('i -> 'j) * ('i -> 'k) -> 'j * 'k) *
   ('l * ('m * 'n) -> ('l * 'm) * 'n))

val qrs : unit ->
  (( ('a,'e) injected -> ('b, 'f) injected -> ('c, 'g) injected -> State.t -> 'd) ->
   State.t ->
   ('a, 'e) refiner * (('b, 'f) refiner * (('c, 'g) refiner * 'd))) *
  (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * ('l -> 'o)) -> 'm * ('n * 'o)) *
   ('p * ('q * ('r * 's)) -> ('p * ('q * 'r)) * 's))

val qrst : unit ->
  ((('a, 'f) injected -> ('b, 'g) injected -> ('c, 'h) injected -> ('d, 'i) injected -> State.t -> 'e) ->
   State.t ->
   ('a, 'f) refiner *
   (('b, 'g) refiner * (('c, 'h) refiner * (('d, 'i) refiner * 'e)))) *
  (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n) *
  (('o ->
    ('o -> 'p) * (('o -> 'q) * (('o -> 'r) * ('o -> 's))) ->
    'p * ('q * ('r * 's))) *
   ('t * ('u * ('v * ('w * 'x))) -> ('t * ('u * ('v * 'w))) * 'x))

val pqrst : unit ->
  ((('a, 'g) injected -> ('b, 'h) injected -> ('c, 'i) injected -> ('d, 'j)  injected -> ('e, 'k) injected -> State.t -> 'f) ->
   State.t ->
   ('a, 'g) refiner *
   (('b, 'h) refiner *
    (('c, 'i) refiner * (('d, 'j) refiner * (('e, 'k) refiner * 'f))))) *
  (('l -> 'm -> 'n -> 'o -> 'p -> 'q) ->
   'l * ('m * ('n * ('o * 'p))) -> 'q) *
  (('r ->
    ('r -> 's) *
    (('r -> 't) * (('r -> 'u) * (('r -> 'v) * ('r -> 'w)))) ->
    's * ('t * ('u * ('v * 'w)))) *
   ('x * ('y * ('z * ('a1 * ('b1 * 'c1)))) ->
    ('x * ('y * ('z * ('a1 * 'b1)))) * 'c1))

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
    type ('a, 'b, 'c) t = ('a, 'b, 'c) T.t
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected) t -> (('a, 'c, 'e) t, ('b, 'd, 'f) t) injected
    val reify : (helper -> ('a, 'b) injected -> 'b) -> (helper -> ('c, 'd) injected -> 'd) -> (helper -> ('e, 'f) injected -> 'f) ->
                helper -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic as 'r) injected -> 'r
  end

module ManualReifiers :
  sig
    val int_reifier: helper -> (int, int logic) injected -> int logic
    val string_reifier: helper -> (string, string logic) injected -> string logic
    val pair_reifier: (helper -> ('a,'b) injected -> 'b) ->
                    (helper -> ('c,'d) injected -> 'd) ->
                    helper -> ('a * 'c, ('b * 'd) logic as 'r) injected -> 'r
 end

(** {2 Standart relational library } *)

(** {3 Predefined types (lists, nats, bools etc.)} *)

(** Abstract list type *)
@type ('a, 'l) llist =
| Nil
| Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr

(** Abstract nat type *)
@type 'a lnat =
| O
| S of 'a with show, html, eq, compare, foldl, foldr, gmap

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
val eqo : ('a, 'b) injected -> ('a, 'b) injected -> Bool.groundi -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b) injected -> ('a, 'b) injected -> Bool.groundi -> goal

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

    (** [of_int n] converts integer [n] into [ground]; negative integers become [O] *)
    val of_int : int -> ground

    (** [to_int g] converts ground [n] into integer *)
    val to_int : ground -> int

    (** A type synonym for injected nat *)
    type groundi = (ground, logic) injected

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

    (** [of_list l] makes a ground list from a regular one *)
    val of_list : ('a, 'b) injected list -> ('a ground, 'b logic) injected

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

    (** Reifier *)
    val reify : (helper -> ('a, 'b) injected -> 'b) -> helper -> ('a ground, 'b logic) injected -> 'b logic

    (** Constructor *)
    val cons : ('a, 'b) injected -> ('a, 'b) groundi -> ('a, 'b) groundi

    (** Relational foldr *)
    val foldro : (('a, 'b) injected -> ('acc, 'acc2) injected -> ('acc, 'acc2) injected -> goal) ->
                 ('acc, 'acc2) injected -> ('a, 'b) groundi -> ('acc, 'acc2) injected -> goal

    (** Relational map *)
    val mapo : (('a, 'b) injected -> ('q, 'w) injected -> goal) -> ('a, 'b) groundi -> ('q, 'w) groundi -> goal

    (** Relational filter *)
    val filtero : (('a, 'b) injected -> Bool.groundi -> goal) -> ('a, 'b) groundi -> ('a, 'b) groundi -> goal

    (** Relational lookup *)
    val lookupo : (('a, 'b) injected -> Bool.groundi -> goal) -> ('a, 'b) groundi -> ('a option, 'b option logic) injected -> goal

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
    val membero : ('a, 'b) groundi  -> ('a, 'b) injected  -> goal

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

(** [inj_list l] is a deforested synonym for injection *)
val inj_list : ('a, 'b) injected list -> ('a, 'b) List.groundi

val inj_pair   : ('a, 'b) injected -> ('c, 'd) injected -> ('a * 'c, ('b * 'd) logic) injected
val inj_list_p : (('a, 'b) injected * ('c, 'd) injected) list -> ('a * 'c, ('b * 'd) logic) List.groundi
val inj_int    : int -> (int, int logic) injected

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
