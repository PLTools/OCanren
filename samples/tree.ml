(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * Tree: binary search tree.
 * Copyright (C) 2022-2023
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
  ocanren type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  type inttree = int tree [@@deriving gt ~options:{show}]
  (* A shortcut for "ground" tree we're going to work with in "functional" code *)
  type rtree = Nat.ground tree [@@deriving gt ~options:{show}]

  (* Logic counterpart *)
  type ltree = Nat.logic tree_logic [@@deriving gt ~options:{show}]

  let leaf    () : Nat.injected tree_injected = inj Leaf
  let node a b c : Nat.injected tree_injected = inj @@ Node (a,b,c)

  (* Injection *)
  let rec inj_tree : inttree -> Nat.injected tree_injected = fun tree ->
     inj @@ GT.(gmap tree_fuly nat inj_tree tree)

  (* Projection *)
  let rec prj_tree : rtree -> inttree =
    fun x -> GT.(gmap tree_fuly) Nat.to_int prj_tree x

  let reify_tree : (Nat.injected tree_injected, ltree) Reifier.t = tree_reify Nat.reify

  let prj_tree : (Nat.injected tree_injected, inttree) Reifier.t =
    let rec tree_to_int x = GT.gmap tree_fuly Nat.to_int ( tree_to_int) x in
    Reifier.fmap tree_to_int (tree_prj_exn Std.Nat.prj_exn)
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
