(*
 * OCanren.
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

(** {3 Logic values} *)

(** A type of a logic value *)
@type 'a logic = private
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr, fmt

(** GT-compatible typeinfo for logics
val logic :
  (unit,
   < show    : ('a -> string) -> 'a logic -> string;
     html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
     eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
     compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
     foldl   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     foldr   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     gmap    : ('a -> 'sa) -> 'a logic -> 'sa logic
   >, unit) GT.t
 *)
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
  method reify: (VarEnv.t -> ('a, 'b) injected -> 'b) -> 'b

  method prjc : (VarEnv.t -> ('a, 'b) injected -> 'a) -> 'a
end

val make_rr : VarEnv.t -> ('a, 'b) injected -> ('a, 'b) reified

(** Functors (1-6 type parameters) *)

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

(* A default shallow reifier *)
val reify : VarEnv.t -> ('a, 'a logic) injected -> 'a logic

val prjc : (int -> 'a list -> 'a) -> VarEnv.t -> ('a, 'a logic) injected -> 'a

val project : ('a, 'b) reified -> 'a
  
(** Building reifiers for a custom type compositionally *)

module Fmap : functor (T : T1) ->
 sig
   val distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected

   val reify : (VarEnv.t -> ('a,'b) injected -> 'b) -> VarEnv.t -> ('a T.t, 'b T.t logic as 'r) injected -> 'r

   val prjc  : (VarEnv.t -> ('a,'b) injected -> 'a) ->
     (int -> 'r list -> ('a T.t as 'r)) ->
     VarEnv.t -> ('r, 'b T.t logic) injected -> 'r
 end

module Fmap2 (T : T2) :
 sig
   val distrib : (('a,'c) injected, ('b,'d) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected

   val reify : (VarEnv.t -> ('a, 'b) injected -> 'b) -> (VarEnv.t -> ('c, 'd) injected -> 'd) -> VarEnv.t -> (('a, 'c) T.t, ('b, 'd) T.t logic as 'r) injected -> 'r

   val prjc  : (VarEnv.t -> ('a, 'b) injected -> 'a) ->
     (VarEnv.t -> ('c, 'd) injected -> 'c) ->
     (int -> 'r list -> (('a,'c) T.t as 'r) ) ->
     VarEnv.t -> ('r, ('b,'d) T.t logic) injected -> 'r
 end

module Fmap3 (T : T3) :
 sig
   val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected

   val reify : (VarEnv.t -> ('a, 'b) injected -> 'b) -> (VarEnv.t -> ('c, 'd) injected -> 'd) -> (VarEnv.t -> ('e, 'f) injected -> 'f) ->
               VarEnv.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic as 'r) injected -> 'r

   val prjc  : (VarEnv.t -> ('a, 'b) injected -> 'a) ->
     (VarEnv.t -> ('c, 'd) injected -> 'c) ->
     (VarEnv.t -> ('e, 'f) injected -> 'e) ->
     (int -> 'r list -> 'r) ->
     VarEnv.t -> (('a,'c,'e) T.t as 'r, ('b,'d,'f) T.t logic) injected -> 'r
 end

module Fmap4 (T : T4) :
 sig
   val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                      (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected

   val reify : (VarEnv.t -> ('a, 'b) injected -> 'b) -> (VarEnv.t -> ('c, 'd) injected -> 'd) ->
               (VarEnv.t -> ('e, 'f) injected -> 'f) -> (VarEnv.t -> ('g, 'h) injected -> 'h) ->
               VarEnv.t -> (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t logic as 'r) injected -> 'r

   val prjc  :
     (VarEnv.t -> ('a, 'b) injected -> 'a) -> (VarEnv.t -> ('c, 'd) injected -> 'c) ->
     (VarEnv.t -> ('e, 'f) injected -> 'e) -> (VarEnv.t -> ('g, 'h) injected -> 'g) ->
     (int -> 'r list -> 'r) ->
     VarEnv.t -> ('r, ('b,'d,'f,'h) T.t logic) injected -> (('a,'c,'e,'g) T.t as 'r)
 end

module Fmap5 (T : T5) :
 sig
   val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                      (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected

   val reify : (VarEnv.t -> ('a, 'b) injected -> 'b) -> (VarEnv.t -> ('c, 'd) injected -> 'd) -> (VarEnv.t -> ('e, 'f) injected -> 'f) ->
               (VarEnv.t -> ('g, 'h) injected -> 'h) -> (VarEnv.t -> ('i, 'j) injected -> 'j) ->
               VarEnv.t -> (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t logic as 'r) injected -> 'r

   val prjc  :
     (VarEnv.t -> ('a, 'b) injected -> 'a) -> (VarEnv.t -> ('c, 'd) injected -> 'c) ->
     (VarEnv.t -> ('e, 'f) injected -> 'e) -> (VarEnv.t -> ('g, 'h) injected -> 'g) ->
     (VarEnv.t -> ('i, 'j) injected -> 'i) ->
     (int -> 'r list -> 'r) ->
     VarEnv.t -> ('r, ('b,'d,'f,'h,'j) T.t logic) injected ->
     (('a,'c,'e,'g,'i) T.t as 'r)
 end

module Fmap6 (T : T6) :
 sig
   val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                      (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected

   val reify : (VarEnv.t -> ('a, 'b) injected -> 'b) -> (VarEnv.t -> ('c, 'd) injected -> 'd) -> (VarEnv.t -> ('e, 'f) injected -> 'f) ->
               (VarEnv.t -> ('g, 'h) injected -> 'h) -> (VarEnv.t -> ('i, 'j) injected -> 'j) -> (VarEnv.t -> ('k, 'l) injected -> 'l) ->
               VarEnv.t -> (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t logic as 'r) injected -> 'r

   val prjc  :
     (VarEnv.t -> ('a, 'b) injected -> 'a) -> (VarEnv.t -> ('c, 'd) injected -> 'c) ->
     (VarEnv.t -> ('e, 'f) injected -> 'e) -> (VarEnv.t -> ('g, 'h) injected -> 'g) ->
     (VarEnv.t -> ('i, 'j) injected -> 'i) -> (VarEnv.t -> ('k, 'l) injected -> 'k) ->
     (int -> 'r list -> 'r) ->
     VarEnv.t -> ('r, ('b,'d,'f,'h,'j,'l) T.t logic) injected ->
     (('a,'c,'e,'g,'i,'k) T.t as 'r)
 end
