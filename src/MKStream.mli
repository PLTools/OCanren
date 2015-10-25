(*
 * MKStream: lazy lists.
 * Copyright (C) 2015
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

(** {2 Implementation of lazy lists} *)

(** Main type: lazy list of 'a-s *)
type 'a t 

(** Exception, which is raised when an end of stream is (accidently)
    reached *)
exception End_of_stream

(** [nil] is an empty stream *)
val nil : 'a t

(** [cons x xs] makes a stream from head [x] and tail [xs] *)
val cons : 'a -> 'a t -> 'a t

(** [destruct s] destructs a non-empty stream *)
val destruct : 'a t -> [`Nil | `Cons of 'a * 'a t]

(** Lazy constructor *)
val from_fun : (unit -> 'a t) -> 'a t

(** Emptiness check *)
val is_empty : 'a t -> bool

(** [hd s] takes a head of non-empty stream *)
val hd : 'a t -> 'a

(** [tl s] takes a tail of non-empty stream *)
val tl : 'a t -> 'a t

(** [concat s1 s2] makes a concatenation of two streams *)
val concat : 'a t -> 'a t -> 'a t

(** [foldl f init s] folds over stream [s] with [f] and [init] *)
val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** [map f s] maps a stream [s] into stream of [f]'s images *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [take ~n:k s] takes at most [k] first elements from [s] *)
val take : ?n:int -> 'a t -> 'a list

(** [concat_map f s] calculates the concatenation of [map f s] *)
val concat_map : ('a -> 'b t) -> 'a t -> 'b t
