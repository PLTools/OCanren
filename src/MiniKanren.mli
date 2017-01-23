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

val generic_show: 'a -> string

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
(** Type [('a,'r)fancy] is a fence between logic values and normal values.
 *  The value of ('a,'r) fancy can be safely obj-magiced from to the type ['r].
 *  For examle:
 *     (int,int) fancy
 *     ((int,'b)llist as 'b, int list) fancy
 *)
type ('a, 'reified) fancy;;

val show_fancy: ('a -> string) -> ('a, 'b) fancy -> string
val bprintf_fancy: Buffer.t -> ('a -> unit) -> ('a, 'b) fancy -> unit
(* call it to unsafely coerce fancy value which doesn't hold logic var *)
val coerce_fancy: ('a, 'r) fancy -> 'a
val cast_fancy  : ('a, 'r) fancy -> 'r

(** A type of abstract logic values *)
type 'a logic = | Var of GT.int GT.list * GT.int * 'a logic GT.list
                | Value of 'a

val index_of_var : 'a logic -> int
val refine_fancy:  ('a,'b) fancy -> (Obj.t -> 'c) -> 'a logic
val refine_fancy2: (_,_) fancy -> (Obj.t -> _) -> _ logic
val var_of_fancy: ('a, 'r) fancy -> 'a logic

val bprintf_logic: Buffer.t -> ('a -> unit) -> 'a logic -> unit
val show_logic: ('a -> string) -> 'a logic -> string
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

val lift : 'a -> ('a, 'a) fancy

(** Injecting values into logics *)
val (!!) : ('a, 'r) fancy -> ('a, 'r) fancy

(** A synonym for [(!!)] *)
val inj : ('a, 'refined) fancy -> ('a, 'refined) fancy;;
(** Exception to raise on a non-value case *)
(* exception Not_a_value *)

(*
(** Projecting logics to values (partial, can raise [Not_a_value]) *)
val (!?) : 'a logic -> 'a

(** A synonym for [(!?)] *)
val prj : 'a logic -> 'a

(** Projection with failure continuation; [prj_k k l] calls continuation [k],
    when a free variable is encountered inside [l]; the number of variable is
    passed to [k] as its argument *)
val prj_k : (int -> 'a logic list -> 'a) -> 'a logic -> 'a
*)
(** {3 Support for some predefined types (lists, nats, bools etc.)} *)

(** Abstract list type *)
@type ('a, 'l) llist = Nil | Cons of 'a * 'l with show, gmap


module type T = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
module type T2 = sig
  type ('a, 'b) t
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end


module Fmap1 (T : T) : sig
  val fmap : ('a, 'c) fancy T.t -> ('a T.t, 'c T.t) fancy
end
module Fmap2 (T : T2) : sig
  type ('a, 'b) t = ('a, 'b) T.t
  val fmap : (('a, 'c) fancy, ('q, 'e) fancy) t ->
             (('a, 'q) t, ('c, 'e) t) fancy
end

(* val lmap : ('a, 'b) fancy -> (('a, 'l) llist as 'l, ('b, 'm) llist as 'm) fancy

val cons : ('a, 'b logic) fancy -> (('a, 'z) llist as 'z, ('b logic, 'c) llist logic as 'c) fancy -> (('a, 'z) llist, ('b logic, 'c) llist logic) fancy

val nil : (('a, 'z) llist as 'z, ('a logic, 'c) llist logic as 'c) fancy  *)


(** Abstract nat type *)
@type 'a lnat = O | S of 'a with show, html, eq, compare, foldl, foldr, gmap

module Bool :
  sig

    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Ground boolean (the regular one) *)
    type ground = bool

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

    (** Logic boolean *)
    type logic = bool logic'

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

    type boolf   = (bool,bool) fancy
    type groundf = boolf
    type fancy   = groundf

    val false_ : boolf
    val true_  : boolf

    (** Sheffer stroke *)
    val (|^) : boolf -> boolf -> boolf -> goal

    (** Negation *)
    val noto' : boolf -> boolf -> goal

    (** Negation as a goal *)
    val noto : boolf -> goal

    (** Disjunction *)
    val oro : boolf -> boolf -> boolf -> goal

    (** Disjunction as a goal *)
    val (||) : boolf -> boolf -> goal

    (** Conjunction *)
    val ando : boolf -> boolf -> boolf -> goal

    (** Conjunction as a goal *)
    val (&&) : boolf -> boolf -> goal

    val show_ground : boolf -> string

    val inj: bool -> boolf
  end

module Nat :
  sig

    (** Type synonym to prevent toplevel [logic] from being hidden *)
    type 'a logic' = 'a logic

    (** Synonym for abstract nat type *)
    type 'a t = 'a lnat

    (** Ground nat are ismorphic for regular one *)
    type ground = ground t

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

    (** Logic nat *)
    type logic = logic t logic'

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
    (* val inj : ground -> (ground,ground) fancy *)

    (** Projection with failure continuation *)
    (* val prj_k : (int -> logic list -> logic t) -> logic -> ground *)

    (** Projection with default continuation *)
    (* val prj : logic -> ground *)

    type groundf = (ground, ground) fancy

    (** Relational addition *)
    val addo  : groundf -> groundf -> groundf -> goal

    (** Infix syninym for [addo] *)
    val ( + ) : groundf -> groundf -> groundf -> goal

    (** Relational multiplication *)
    val mulo  : groundf -> groundf -> groundf -> goal

    (** Infix syninym for [mulo] *)
    val ( * ) : groundf -> groundf -> groundf -> goal

    (** Comparisons *)
    (* val leo : logic -> logic -> Bool.logic -> goal
    val geo : logic -> logic -> Bool.logic -> goal
    val gto : logic -> logic -> Bool.logic -> goal
    val lto : logic -> logic -> Bool.logic -> goal *)

    (** Comparisons as goals *)
    (* val (<=) : logic -> logic -> goal
    val (>=) : logic -> logic -> goal *)
    (* val (>)  : logic -> logic -> goal
    val (<)  : logic -> logic -> goal *)

    val show_ground: (ground,ground) fancy -> string
  end

(** [inj_nat n] is a deforested synonym for injection *)
val inj_nat : int -> (Nat.ground, Nat.ground) fancy

(** [prj_nat n] is a deforested synonym for projection *)
(* val prj_nat : Nat.logic -> int *)

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

    (** GT-compatible typeinfo for ['a ground] *)
    val ground :
      (unit,
       < gmap    : ('a -> 'b) -> 'a ground -> 'b ground;
         (* compare : ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
         foldl   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
         foldr   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
         html    : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer; *)
         show    : ('a -> string) -> 'a ground -> string >)
      GT.t

    (** [of_list l] makes ground list from a regular one *)
    val of_list : ('a, 'c) fancy list ->
           (('a, 'd) t as 'd, ('c, 'f) t as 'f) fancy

    (** [to_list l] make regular list from a ground one *)
    (* val to_list : 'a ground -> 'a list *)

    (** Logic lists (with the tails as logic lists) *)
    type 'a logic  = ('a, 'a logic)  t logic'

    (** GT-compatible typeinfo for ['a logic] *)
    val logic :
      (unit,
       < gmap    : ('a -> 'b) -> (('a, 'c) t logic' as 'c) -> (('b, 'd) t logic' as 'd);
         (* compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
         eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
         foldr   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
         foldl   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
         html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer; *)
         show    : ('a -> string) -> 'a logic -> GT.string  >)
      GT.t

    val cons :
             ('a, 'c) fancy ->
             ('d, 'f) fancy -> (('a, 'd) t, ('c, 'f) t logic') fancy
(*
    (** List injection *)
    val inj : ('a -> 'b) -> 'a ground -> 'b logic

    (** List projection with failure continuation *)
    val prj_k : ('a -> 'b) -> (int -> 'a logic list -> ('a, 'a logic) t) ->  'a logic -> 'b ground

    (** List projection with default continuation *)
    val prj : ('a -> 'b) -> 'a logic -> 'b ground
*)
    type ('a, 'b) flist = (('a,'b) fancy ground, 'b list) fancy

    (** Relational foldr *)
    val foldro : (('a, 'a2) fancy -> ('acc,'accr) fancy -> ('acc,'accr) fancy -> goal) ->
                 ('acc,'accr) fancy -> ('a,'a2) flist -> ('acc,'accr) fancy ->  goal

    (** Relational map *)
    val mapo : (('a, 'b) fancy -> ('c, 'd) fancy -> goal) -> ('a, 'b) flist -> ('c, 'd) flist -> goal

    (** Relational filter *)
    val filtero : (('a, 'b) fancy -> Bool.groundf -> goal) -> ('a, 'b) flist -> ('a, 'b) flist -> goal

    (** Relational lookup *)
    val lookupo : (('a, 'b) fancy -> Bool.groundf -> goal) ->  ('a, 'b) flist -> ('a option, 'b option) fancy -> goal

    (** Boolean list disjunctions *)
    val anyo : (bool,bool) flist -> Bool.groundf -> goal

    (** Boolean list conjunction *)
    val allo : (bool,bool) flist -> Bool.groundf -> goal

    (** Relational length *)
    val lengtho : ('a, 'b) flist -> Nat.groundf -> goal

    (** Relational append *)
    val appendo : ('a, 'b) flist -> ('a, 'b) flist  -> ('a, 'b) flist  -> goal

    (** Relational reverse *)
    val reverso : ('a, 'b) flist  -> ('a, 'b) flist  -> goal

    (** Relational occurrence check (a shortcut) *)
    val membero : ('a, 'b) flist  -> ('a, 'b) fancy  -> goal

    val nullo : (('a,'b) llist as 'b, 'c list) fancy -> goal
    (** Relational head of the list *)
    val caro  : ((('a,'b) fancy, 'tl) llist as 'tl, 'b list) fancy -> ('a, 'b) fancy -> goal
    (** Relational tail of the list *)
    val cdro  : ((('a,'b) fancy, 'tl) llist as 'tl, 'b list) fancy -> ('tl, 'b list) fancy -> goal
    (** Alias for [caro] *)
    val hdo  : ((('a,'b) fancy, 'tl) llist as 'tl, 'b list) fancy -> ('a, 'b) fancy -> goal
    (** Alias for [cdro] *)
    val tlo   : ((('a,'b) fancy, 'tl) llist as 'tl, 'b list) fancy -> ('tl, 'b list) fancy -> goal

    val show : ('a -> string) -> (('a, 'b) llist as 'b,_) fancy -> string
  end

(** [inj_list l] is a deforested synonym for injection *)
val inj_list : ('a,'b) fancy list -> (( ('a,'b) fancy,'c) llist as 'c, 'b list) fancy

val inj_pair : ('a, 'b) fancy -> ('c,'d) fancy -> ('a*'c, 'b*'d) fancy
val inj_list_p : (('a, 'b) fancy * ('c, 'd) fancy) list ->
         ((('a*'c, 'b*'d) fancy, 'e) llist as 'e, 'e) fancy

(** [prj_list] is a deforested synonym for projection *)
(* val prj_list : 'a logic List.logic -> 'a list *)

(** [inj_nat_list l] is a deforsted synonym for injection *)
val inj_nat_list : int list -> (( (Nat.ground, Nat.ground) fancy, 'tl) llist as 'tl, Nat.ground list) fancy

(** [inj_nat_list l] is a deforsted synonym for projection *)
(* val prj_nat_list : Nat.logic List.logic -> int list *)

(** Infix synonym for [Cons] *)
val (%) :   ('a,'c) fancy ->
          ((('a,'c) fancy,'b) llist as 'b, 'c list) fancy ->
          ((('a,'c) fancy,'b) llist as 'b, 'c list) fancy

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'r) fancy ->
           ('a, 'r) fancy ->
           ((('a, 'r) fancy, 'b) llist as 'b, 'r list) fancy

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'r) fancy ->
           ((('a, 'r) fancy, 'b) llist as 'b, 'r list) fancy

(** [nil] is a synonym for [inj Nil] *)
val nil : unit -> ((('a, 'r) fancy, 'b) llist as 'b, 'r list) fancy

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'c) fancy -> State.t -> 'r) -> State.t -> 'r

(** [x === y] creates a goal, which performs a unifications of
    [x] and [y] *)
val (===) : ('a, 'c) fancy -> ('a, 'c) fancy -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for
    [x] and [y] *)
val (=/=) : ('a, 'b) fancy -> ('a, 'b) fancy -> goal

(** Equality as boolean relation *)
val eqo  : ('a, 'b) fancy -> ('a, 'b) fancy -> Bool.fancy -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b) fancy -> ('a, 'b) fancy -> Bool.fancy -> goal

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
    val succ : ('a -> State.t -> 'b) -> (('c, 'd) fancy -> 'a) -> State.t -> 'b

    (** Zero logic parameters *)
    val zero : 'a -> 'a

    (** {3 One to five logic parameter(s)} *)

    val one   : ((_,_) fancy -> State.t -> 'r) -> State.t -> 'r
    val two   : ((_,_) fancy -> (_,_) fancy -> State.t -> 'r) -> State.t -> 'r

    val three : ((_,_) fancy -> (_,_) fancy -> (_,_) fancy -> State.t -> 'r) -> State.t -> 'r
    val four  : ((_,_) fancy -> (_,_) fancy -> (_,_) fancy -> (_,_) fancy ->
                State.t -> 'r) -> State.t -> 'r
    val five  : ((_,_) fancy -> (_,_) fancy -> (_,_) fancy -> (_,_) fancy ->
                 (_,_) fancy -> State.t -> 'r) -> State.t -> 'r

    (** {3 One to five logic parameter(s), conventional names} *)

    val q     : ((_,_) fancy -> State.t -> 'r) -> State.t -> 'r
    val qr    : ((_,_) fancy -> (_,_) fancy -> State.t -> 'r) -> State.t -> 'r

    val qrs   : ((_,_) fancy -> (_,_) fancy -> (_,_) fancy -> State.t -> 'r) -> State.t -> 'r
    (* val qrst  : (('a, 'b logic logic) fancy -> ('c, 'd logic logic) fancy -> ('e, 'f logic logic) fancy -> ('g, 'h logic logic) fancy ->                         State.t -> 'i) -> State.t -> 'i
    val pqrst : (('a, 'b logic logic) fancy -> ('c, 'd logic logic) fancy -> ('e, 'f logic logic) fancy -> ('g, 'h logic logic) fancy -> ('i, 'j logic logic) fancy -> State.t -> 'k) -> State.t -> 'k *)

  end

(** {2 Top-level running primitives} *)

exception WithFreeVars of (Obj.t -> bool) * Obj.t

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

type ('a,'b) reification_rez = Final of 'b | HasFreeVars of ((Obj.t -> bool) * Obj.t)
(** Some type to refine a stream of states into the stream of answers (w.r.t. some known
    logic variable *)
type ('a,'b) refiner = State.t Stream.t -> ('a, 'b) reification_rez Stream.t

(** Successor function *)
val succ :
  (unit -> ('a -> State.t -> 'b) * ('c -> 'd -> 'e) * (('f -> 'g -> 'h) * ('i -> 'j * 'k))) ->
  (unit -> ((('l, 'z) fancy -> 'a) -> State.t -> ('l,'z) refiner * 'b) * (('m -> 'c) -> 'm * 'd -> 'e) * (('f -> ('f -> 'n) * 'g -> 'n * 'h) * ('o * 'i -> ('o * 'j) * 'k)))


(** {3 Predefined numerals (one to five)} *)

val one :
  unit ->
  ((('a, 'b) fancy -> State.t -> 'c) -> State.t -> ('a, 'b) refiner * 'c) *
  (('d -> 'e) -> 'd -> 'e) * (('f -> ('f -> 'g) -> 'g) * ('h -> 'h))

val two :
  unit ->
  ((('a, 'b) fancy -> ('c, 'd) fancy -> State.t -> 'e) ->
   State.t -> ('a, 'b logic) refiner * (('c, 'd logic) refiner * 'e)) *
  (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
  (('i -> ('i -> 'j) * ('i -> 'k) -> 'j * 'k) *
   ('l * ('m * 'n) -> ('l * 'm) * 'n))
   (*
val three :
  unit ->
  ((('a, 'b logic) fancy ->
    ('c, 'd logic) fancy -> ('e, 'f logic) fancy -> State.t -> 'g) ->
   State.t -> 'a refiner * ('c refiner * ('e refiner * 'g))) *
  (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * ('l -> 'o)) -> 'm * ('n * 'o)) *
   ('p * ('q * ('r * 's)) -> ('p * ('q * 'r)) * 's))
val four :
  unit ->
  ((('a, 'b logic) fancy ->
    ('c, 'd logic) fancy ->
    ('e, 'f logic) fancy -> ('g, 'h logic) fancy -> State.t -> 'i) ->
   State.t -> 'a refiner * ('c refiner * ('e refiner * ('g refiner * 'i)))) *
  (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n) *
  (('o ->
    ('o -> 'p) * (('o -> 'q) * (('o -> 'r) * ('o -> 's))) ->
    'p * ('q * ('r * 's))) *
   ('t * ('u * ('v * ('w * 'x))) -> ('t * ('u * ('v * 'w))) * 'x))
val five :
  unit ->
  ((('a, 'b logic) fancy ->
    ('c, 'd logic) fancy ->
    ('e, 'f logic) fancy ->
    ('g, 'h logic) fancy -> ('i, 'j logic) fancy -> State.t -> 'k) ->
   State.t ->
   'a refiner *
   ('c refiner * ('e refiner * ('g refiner * ('i refiner * 'k))))) *
  (('l -> 'm -> 'n -> 'o -> 'p -> 'q) -> 'l * ('m * ('n * ('o * 'p))) -> 'q) *
  (('r ->
    ('r -> 's) * (('r -> 't) * (('r -> 'u) * (('r -> 'v) * ('r -> 'w)))) ->
    's * ('t * ('u * ('v * 'w)))) *
   ('x * ('y * ('z * ('a1 * ('b1 * 'c1)))) ->
    ('x * ('y * ('z * ('a1 * 'b1)))) * 'c1)) *)

(** {3 The same numerals with conventional names} *)

val q :
  unit ->
  ((('a, 'r) fancy -> State.t -> 'c) -> State.t -> ('a, 'r) refiner * 'c) *
  (('d -> 'e) -> 'd -> 'e) * (('f -> ('f -> 'g) -> 'g) * ('h -> 'h))
val qr :
  unit ->
  ((('a, 'b) fancy -> ('c, 'd) fancy -> State.t -> 'e) ->
   State.t -> ('a, 'b) refiner * (('c, 'd) refiner * 'e)) *
  (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
  (('i -> ('i -> 'j) * ('i -> 'k) -> 'j * 'k) *
   ('l * ('m * 'n) -> ('l * 'm) * 'n))
 val qrs :
  unit ->
  ((('a, 'b) fancy -> ('c, 'd) fancy -> ('e, 'f) fancy -> State.t -> 'g) ->
   State.t -> ('a, 'b) refiner * (('c, 'd) refiner * (('e, 'f) refiner * 'g))) *
  (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
  (('l -> ('l -> 'm) * (('l -> 'n) * ('l -> 'o)) -> 'm * ('n * 'o)) *
   ('p * ('q * ('r * 's)) -> ('p * ('q * 'r)) * 's))
 val qrst :
  unit ->
  ((('a, 'b) fancy ->
    ('c, 'd) fancy ->
    ('e, 'f) fancy -> ('g, 'h) fancy -> State.t -> 'i) ->
   State.t -> ('a, 'b) refiner * (('c, 'd) refiner * (('e, 'f) refiner * (('g, 'h) refiner * 'i)))) *
  (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n) *
  (('o ->
    ('o -> 'p) * (('o -> 'q) * (('o -> 'r) * ('o -> 's))) ->
    'p * ('q * ('r * 's))) *
   ('t * ('u * ('v * ('w * 'x))) -> ('t * ('u * ('v * 'w))) * 'x))

val pqrst :
  unit ->
  ((('a, 'b) fancy ->
    ('c, 'd) fancy ->
    ('e, 'f) fancy ->
    ('g, 'h) fancy -> ('i, 'j) fancy -> State.t -> 'k) ->
   State.t ->
   ('a, 'b) refiner * (('c, 'd) refiner * (('e, 'f) refiner * (('g, 'h) refiner * (('i, 'j) refiner * 'k))))) *
  (('l -> 'm -> 'n -> 'o -> 'p -> 'q) -> 'l * ('m * ('n * ('o * 'p))) -> 'q) *
  (('r ->
    ('r -> 's) * (('r -> 't) * (('r -> 'u) * (('r -> 'v) * ('r -> 'w)))) ->
    's * ('t * ('u * ('v * 'w)))) *
   ('x * ('y * ('z * ('a1 * ('b1 * 'c1)))) ->
    ('x * ('y * ('z * ('a1 * 'b1)))) * 'c1))

(*
module type X1 = sig
type t1
type t2
end
module FMapALike(T: X1) : sig
  external wrap : (T.t1, T.t1) fancy -> (T.t1, T.t2) fancy = "%identity"
end *)


module type Y0 = sig
  type t
  type r
end
module FMapALike0(Y: Y0) : sig
  external wrap : (Y.t, Y.t) fancy -> (Y.t, Y.r) fancy = "%identity"
end

module type Y1 = sig
  type 'a t
  type r
end
module FMapALike2(Y: Y1) : sig
  external wrap : ('a Y.t, 'a Y.t) fancy -> ('a Y.t, Y.r) fancy = "%identity"
end
