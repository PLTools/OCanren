(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * Tree: binary search tree.
 * Copyright (C) 2022
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
open OCanren
open OCanren.Std

module Tree = struct

  [%%distrib
    (* Abstracted type for the tree *)
    type nonrec ('a, 'self) t = Leaf | Node of 'a * 'self * 'self [@@deriving gt ~options:{show; gmap}];;
    type 'a ground = ('a, 'a ground) t
  ]

  type inttree = (int, inttree) t [@@deriving gt ~options:{show}]
  (* A shortcut for "ground" tree we're going to work with in "functional" code *)
  type rtree = (Nat.ground, rtree) t [@@deriving gt ~options:{show}]

  (* Logic counterpart *)
  type ltree = (Nat.logic, ltree) t OCanren.logic [@@deriving gt ~options:{show}]

  type injected = (Nat.injected, injected) t OCanren.ilogic

  let leaf    () : injected = inj Leaf
  let node a b c : injected = inj @@ Node (a,b,c)

  (* Injection *)
  let rec inj_tree : inttree -> injected = fun tree ->
     inj @@ GT.(gmap t nat inj_tree tree)

  (* Projection *)
  let rec prj_tree : rtree -> inttree =
    fun x -> GT.(gmap t) Nat.to_int prj_tree x

  let reify_tree : (injected, ltree) Reifier.t = reify Nat.reify
  (* let rec prjc_tree env t = M.prjc Nat.prjc prjc_tree env t *)

  let prj_tree : (injected, inttree) Reifier.t =
    let rec tree_to_int x = GT.gmap t Nat.to_int ( tree_to_int) x in
    Reifier.fmap tree_to_int (prj_exn Std.Nat.prj_exn)
end

open Tree


let () =
  (* Demo about reification *)

  let _: Tree.ltree Stream.t =
    run q (fun q  -> q === leaf ())
        (fun qs -> qs#reify Tree.reify_tree)
  in
  (* run q (fun q  -> q === leaf ())
        (fun qs -> qs#reify Tree.prjc_tree) *)
  ()



(* Relational insert into a search tree *)
let rec inserto a t' t'' = conde [
  (t' === leaf ()) &&& (t'' === node a (leaf ()) (leaf ()) );
  fresh (x l r l')
    (t' === node x l r)
    Nat.(conde [
      (t'' === t') &&& (a === x);
      (t'' === (node x l' r  )) &&& (a < x) &&& (inserto a l l');
      (t'' === (node x l  l' )) &&& (a > x) &&& (inserto a r l')
    ])
]

(* Top-level wrapper for insertion --- takes and returns non-logic data *)
let insert : int -> inttree -> inttree = fun a t ->
  Stream.hd @@
  run q (fun q  -> inserto (nat a) (inj_tree t) q)
        (fun qs -> qs#reify Tree.prj_tree)

(* Top-level wrapper for "inverse" insertion --- returns an integer, which
   has to be inserted to convert t into t' *)
let insert' t t' =
  Nat.to_int @@ Stream.hd @@
  run q (fun q  -> inserto q (inj_tree t) (inj_tree t'))
        (fun qs -> qs#reify Nat.prj_exn)

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
    inner Leaf l
  in
  ignore @@ insert_list [1; 2; 3; 4];
  let t  = insert_list [3; 2; 4; 1] in
  let t' = insert 8 t in
  Printf.printf "Inverse insert: %d\n" @@ insert' t t'
