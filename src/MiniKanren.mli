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

    (** A type of the stream *)
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

(** Goal converts a state into lazy stream of states *)
type goal = State.t -> State.t Stream.t

(** {3 Logics} *)

(** Type [('a, 'b) injected] is a fence between logic values and normal values.
 *)
type ('a,'b) injected

(** A type of abstract logic values *)
@type 'a logic = 
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

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

val lift : 'a -> ('a, 'a) injected
val inj  : ('a, 'b) injected -> ('a, 'b logic) injected

(** A synonym for [inj] *)
val (!!) : ('a, 'b) injected -> ('a, 'b) injected

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'b) injected -> State.t -> 'r) -> State.t -> 'r

(** [x === y] creates a goal, which performs a unifications of
    [x] and [y] *)
val (===) : ('a, 'b) injected -> ('a, 'b) injected -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for
    [x] and [y] *)
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
    non-empty list of goals *)
val (?|) : goal list -> goal

(** [conde] is a synonym for [?|] *)
val conde : goal list -> goal

(** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a
    non-empty list of goals *)
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

type var_checker
type ('a, 'b) reification_rez = Final of 'a | HasFreeVars of var_checker * ('a, 'b) injected

(** Some type to refine a stream of states into the stream of answers (w.r.t. some known
    logic variable *)
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
module type T1 = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
module type T2 = sig
  type ('a, 'b) t
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end
module type T3 = sig
  type ('a, 'b, 'c) t
  val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
end

module Fmap1 (T : T1) : sig
  val distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected

  val reifier:
    (var_checker -> ('a,'b) injected -> 'b) ->
    var_checker -> ('a T.t, 'b T.t logic as 'r) injected -> 'r
end
module Fmap2 (T : T2) : sig
  val distrib   : (('a,'c) injected, ('b,'d) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected

  val reifier:
    (var_checker -> ('a, 'b) injected -> 'b) ->
    (var_checker -> ('c, 'd) injected -> 'd) ->
    var_checker -> (('a, 'c) T.t, ('b, 'd) T.t logic as 'r) injected -> 'r

end
module Fmap3 (T : T3) : sig
  type ('a, 'b, 'c) t = ('a, 'b, 'c) T.t
  val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected) t -> (('a, 'c, 'e) t, ('b, 'd, 'f) t) injected

  val reifier:
    (var_checker -> ('a, 'b) injected -> 'b) ->
    (var_checker -> ('c, 'd) injected -> 'd) ->
    (var_checker -> ('e, 'f) injected -> 'f) ->
    var_checker -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic as 'r) injected -> 'r
end

module ManualReifiers : sig
  val int_reifier: var_checker -> (int, int logic) injected -> int logic
  val string_reifier: var_checker -> (string, string logic) injected -> string logic
  val pair_reifier: (var_checker -> ('a,'b) injected -> 'b) ->
                    (var_checker -> ('c,'d) injected -> 'd) ->
                    var_checker -> ('a * 'c, ('b * 'd) logic as 'r) injected -> 'r
end

(** {2 Standart relational library } *)

(** {3 Support for some predefined types (lists, nats, bools etc.)} *)

(** Abstract list type *)
@type ('a, 'l) llist = 
| Nil 
| Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr

(** Abstract nat type *)
@type 'a lnat = 
| O 
| S of 'a with show, html, eq, compare, foldl, foldr, gmap

(** {3 Library itself } *)
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

    type groundf = (ground, logic) injected

    val false_ : groundf
    val true_  : groundf

    (** Sheffer stroke *)
    val (|^) : groundf -> groundf -> groundf -> goal

    (** Negation *)
    val noto' : groundf -> groundf -> goal

    (** Negation as a goal *)
    val noto : groundf -> goal

    (** Disjunction *)
    val oro : groundf -> groundf -> groundf -> goal

    (** Disjunction as a goal *)
    val (||) : groundf -> groundf -> goal

    (** Conjunction *)
    val ando : groundf -> groundf -> groundf -> goal

    (** Conjunction as a goal *)
    val (&&) : groundf -> groundf -> goal

    (** Injection *)
    val inj: bool -> groundf
  end

(** Equality as boolean relation *)
val eqo : ('a, 'b) injected -> ('a, 'b) injected -> Bool.groundf -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b) injected -> ('a, 'b) injected -> Bool.groundf -> goal

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

    val reifier : var_checker -> (ground, logic) injected -> logic

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

    (** [of_int n] converts integer [n] into [ground]; negtive
        integers become [O] *)
    val of_int : int -> ground

    (** [to_int g] converts ground [n] into integer *)
    val to_int : ground -> int

    (** [inj n] converts ground nat [n] into logic one *)
    (* val inj : ground -> (ground,ground) injected *)

    (** Projection with failure continuation *)
    (* val prj_k : (int -> logic list -> logic t) -> logic -> ground *)

    (** Projection with default continuation *)
    (* val prj : logic -> ground *)

    type groundf = (ground, logic) injected

    (** Relational addition *)
    val addo  : groundf -> groundf -> groundf -> goal

    (** Infix syninym for [addo] *)
    val ( + ) : groundf -> groundf -> groundf -> goal

    (** Relational multiplication *)
    val mulo  : groundf -> groundf -> groundf -> goal

    (** Infix syninym for [mulo] *)
    val ( * ) : groundf -> groundf -> groundf -> goal

    (** Comparisons *)
    val leo : groundf -> groundf -> Bool.groundf -> goal
    val geo : groundf -> groundf -> Bool.groundf -> goal
    val gto : groundf -> groundf -> Bool.groundf -> goal
    val lto : groundf -> groundf -> Bool.groundf -> goal

    (** Comparisons as goals *)
    val (<=) : groundf -> groundf -> goal
    val (>=) : groundf -> groundf -> goal
    val (>)  : groundf -> groundf -> goal
    val (<)  : groundf -> groundf -> goal
  end

(** [inj_nat n] is a deforested synonym for injection *)
val inj_nat : int -> (Nat.ground, Nat.logic) injected

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

    (** [of_list l] makes ground list from a regular one *)
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

    type ('a,'b) flist = ('a ground, 'b logic) injected

    val flist :
      (unit,
        < show    : ('a -> string) -> ('a,_) flist -> GT.string  >)
        GT.t

    val reifier: (var_checker -> ('a, 'b) injected -> 'b) -> var_checker -> ('a ground, 'b logic) injected -> 'b logic

    val cons : ('a, 'b) injected -> ('a,'b) flist -> ('a,'b) flist
(*
    (** List injection *)
    val inj : ('a -> 'b) -> 'a ground -> 'b logic

    (** List projection with failure continuation *)
    val prj_k : ('a -> 'b) -> (int -> 'a logic list -> ('a, 'a logic) t) ->  'a logic -> 'b ground

    (** List projection with default continuation *)
    val prj : ('a -> 'b) -> 'a logic -> 'b ground
*)


    (** Relational foldr *)
    val foldro : (('a, 'b) injected -> ('acc, 'acc2) injected -> ('acc, 'acc2) injected -> goal) ->
                 ('acc, 'acc2) injected -> ('a, 'b) flist -> ('acc, 'acc2) injected -> goal

    (** Relational map *)
    val mapo : (('a, 'b) injected -> ('q, 'w) injected -> goal) -> ('a, 'b) flist -> ('q, 'w) flist -> goal

    (** Relational filter *)
    val filtero : (('a, 'b) injected -> Bool.groundf -> goal) -> ('a, 'b) flist -> ('a, 'b) flist -> goal

    (** Relational lookup *)
    val lookupo : (('a, 'b) injected -> Bool.groundf -> goal) -> ('a, 'b) flist -> ('a option, 'b option logic) injected -> goal

    (** Boolean list disjunctions *)
    val anyo : (Bool.ground, Bool.logic) flist -> Bool.groundf -> goal

    (** Boolean list conjunction *)
    val allo : (Bool.ground, Bool.logic) flist -> Bool.groundf -> goal

    (** Relational length *)
    val lengtho : (_, _) flist -> Nat.groundf -> goal

    (** Relational append *)
    val appendo : ('a, 'b) flist -> ('a, 'b) flist  -> ('a, 'b) flist -> goal

    (** Relational reverse *)
    val reverso : ('a, 'b) flist -> ('a, 'b) flist -> goal

    (** Relational occurrence check (a shortcut) *)
    val membero : ('a, 'b) flist  -> ('a, 'b) injected  -> goal

    (** Relational check for empty list *)
    val nullo : _ flist -> goal

    (** Relational head of the list *)
    val caro  : ('a, 'b) flist -> ('a, 'b) injected -> goal

    (** Alias for [caro] *)
    val hdo   : ('a, 'b) flist -> ('a, 'b) injected -> goal

    (** Relational tail of the list *)
    val cdro  : ('a, 'b) flist -> ('a, 'b) flist -> goal

    (** Alias for [cdro] *)
    val tlo   : ('a, 'b) flist -> ('a, 'b) flist -> goal

    (* val show : ('a -> string) -> ('a,_) flist -> string *)
  end

(** [inj_list l] is a deforested synonym for injection *)
val inj_list : ('a, 'b) injected list -> ('a, 'b) List.flist

val inj_pair   : ('a, 'b) injected -> ('c, 'd) injected -> ('a * 'c, ('b * 'd) logic) injected
val inj_list_p : (('a, 'b) injected * ('c, 'd) injected) list -> ('a * 'c, ('b * 'd) logic) List.flist
val inj_int    : int -> (int, int logic) injected

(** [inj_nat_list l] is a deforsted synonym for injection *)
val inj_nat_list : int list -> (Nat.ground, Nat.logic) List.flist

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) injected -> ('a,'b) List.flist -> ('a,'b) List.flist

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) List.flist

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) injected -> ('a, 'b) List.flist

(** [nil] is a synonym for [inj Nil] *)
val nil : unit -> (_, _) List.flist
