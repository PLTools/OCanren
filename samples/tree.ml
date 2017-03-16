(*
 * Tree: binary search tree.
 * Copyright (C) 2016
 * Dmitri Boulytchev, Dmitrii Kosarev
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

open Printf
open GT
open MiniKanren

module Tree = struct
  module X = struct
  (* Abstracted type for the tree *)
  @type ('a, 'self) t = Nil | Node of 'a * 'self * 'self with gmap,show;;

  let fmap f g = function
  | Nil -> Nil
  | Node (a,b,c) -> Node (f a, g b, g c)
  end
  include X
  include Fmap2(X)

  type inttree = (int, inttree) X.t
  (* A shortcut for "ground" tree we're going to work with in "functional" code *)
  type rtree = (Nat.ground, rtree) X.t

  (* Logic counterpart *)
  type ltree = (Nat.logic, ltree) X.t logic

  type ftree = (rtree, ltree) injected

  let nil        : ftree = inj @@ distrib @@ X.Nil
  let node a b c : ftree = inj @@ distrib @@ X.Node (a,b,c)

  (* Printing tree with ints inside *)
  let rec show_inttree t = GT.(show X.t (show int) show_inttree) t
  (* Printing tree with Peano numbers inside *)
  let rec show_rtree t = GT.(show X.t (show Nat.ground) show_rtree) t
  (* Printing logical tree *)
  let rec show_ltree t = GT.(show logic @@ show X.t (show Nat.logic) show_ltree) t

  (* Injection *)
  let rec inj_tree : inttree -> ftree = fun tree ->
     inj @@ distrib @@ GT.(gmap t inj_nat inj_tree tree)

  (* Projection *)
  let rec prj_tree : rtree -> inttree =
    fun x -> GT.(gmap t) Nat.to_int prj_tree x

end

open Tree

(* Relational insert into a search tree *)
let rec inserto a t t' = conde [
  (t === nil) &&& (t' === node a nil nil);
  fresh (x l r l')
    (t === node x l r)
    Nat.(conde [
      (t' === t) &&& (a === x);
      (t' === (node x l' r  )) &&& (a < x) &&& (inserto a l l');
      (t' === (node x l  l' )) &&& (a > x) &&& (inserto a r l')
    ])
]

(* Top-level wrapper for insertion --- takes and returns non-logic data *)
let insert : int -> inttree -> inttree = fun a t ->
  run q (fun q  -> inserto (inj_nat a) (inj_tree t) q)
        (fun qs -> prj_tree (Stream.hd qs)#prj)

(* Top-level wrapper for "inverse" insertion --- returns an integer, which
   has to be inserted to convert t into t' *)
let insert' t t' =
  run q (fun q  -> inserto q (inj_tree t) (inj_tree t'))
        (fun qs -> Nat.prj_ground (Stream.hd qs)#prj)

(* Entry point *)
let _ =
  let insert_list l =
    let rec inner t = function
    | []    -> t
    | x::xs ->
      let t' = insert x t in
      printf "Inserting %d into %s makes %s\n%!" x (show_inttree t) (show_inttree t');
      inner t' xs
    in
    inner Nil l
  in
  ignore @@ insert_list [1; 2; 3; 4];
  let t  = insert_list [3; 2; 4; 1] in
  let t' = insert 8 t in
  Printf.printf "Inverse insert: %d\n" @@ insert' t t'
