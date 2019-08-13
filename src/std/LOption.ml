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

open Logic
open Core

type 'a logic' = 'a logic

let logic' = logic

type 'a ground = 'a option

let ground = GT.option

type 'a logic  = 'a option logic'

let ground = {
  GT.gcata = ();
  GT.fix = ();
  GT.plugins =
    object(this)
      method html    n   = GT.html   (GT.option) n
      method eq      n m = GT.eq     (GT.option) n m
      method compare n m = GT.compare(GT.option) n m
      method foldr   n   = GT.foldr  (GT.option) n
      method foldl   n   = GT.foldl  (GT.option) n
      method gmap    n   = GT.gmap   (GT.option) n
      method show    n   = GT.show   (GT.option) n
    end
}

let logic = {
  GT.gcata = ();
  GT.fix = ();
  GT.plugins =
    object(this)
      method html    f n   = GT.html   (logic') (GT.html   (ground) f) n
      method eq      f n m = GT.eq     (logic') (GT.eq     (ground) f) n m
      method compare f n m = GT.compare(logic') (GT.compare(ground) f) n m
      method foldr   f a n = GT.foldr  (logic') (GT.foldr  (ground) f) a n
      method foldl   f a n = GT.foldl  (logic') (GT.foldl  (ground) f) a n
      method gmap    f n   = GT.gmap   (logic') (GT.gmap   (ground) f) n
      method show    f n   = GT.show   (logic') (GT.show   (ground) f) n
    end
}

let inj f x = to_logic (GT.(gmap option) f x)

type ('a, 'b) groundi = ('a ground, 'b logic) injected

module T =
  struct
    type 'a t = 'a option
    let fmap f x = GT.(gmap option) f x
  end

include T
include Fmap (T)

let some x  = Logic.inj @@ distrib (Some x)
let none () = Logic.inj @@ distrib None

let option = function None -> none () | Some x -> some x
