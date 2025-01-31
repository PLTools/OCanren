(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * Tree: binary search tree.
 * Copyright (C) 2022-2025
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

open OCanren

module Tree = struct
  ocanren type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  type inttree = GT.int tree [@@deriving gt ~options:{show}]
  (* A shortcut for "ground" tree we're going to work with in "functional" code *)
  type rtree = Std.Nat.ground tree [@@deriving gt ~options:{show}]

  (* Logic counterpart *)
  type ltree = Std.Nat.logic tree_logic [@@deriving gt ~options:{show}]

  let leaf    () : Std.Nat.injected tree_injected = inj Leaf
  let node a b c : Std.Nat.injected tree_injected = inj @@ Node (a,b,c)

  (* Injection *)
  let rec inj_tree : inttree -> Std.Nat.injected tree_injected = fun tree ->
     inj @@ GT.(gmap tree_fuly Std.nat inj_tree tree)

  (* Projection *)
  let rec prj_tree : rtree -> inttree =
    fun eta -> GT.(gmap tree_fuly) Std.Nat.to_int prj_tree eta

  let reify_tree : (Std.Nat.injected tree_injected, ltree) Reifier.t =
    tree_reify Std.Nat.reify

  let prj_exn_tree : (Std.Nat.injected tree_injected, inttree) Reifier.t =
    let rec tree_to_int x = GT.gmap tree_fuly Std.Nat.to_int (tree_to_int) x in
    Reifier.fmap tree_to_int (tree_prj_exn Std.Nat.prj_exn)
end

let () =
  let open Tree in
  (* Demo about full blown reification *)
  let answers: Tree.ltree Stream.t =
    run q (fun q -> q === leaf ())
        (fun qs -> qs#reify Tree.reify_tree)
  in
  assert (Stream.take answers = [Value Leaf]);
  (* reification to ground representation *)
  let answers: Tree.inttree Stream.t =
    run q (fun q -> q === leaf ())
        (fun qs -> qs#reify Tree.prj_exn_tree)
  in
  assert (Stream.take answers = [Leaf])

(* Relational insert into a search tree *)
let rec inserto a t' t'' =
  let open Tree in
  conde
    [ (t' === leaf ()) &&& (t'' === node a (leaf ()) (leaf ()))
    ; fresh (x l r l')
        (t' === node x l r)
        (conde [
          (t'' === t') &&& (a === x);
          (t'' === node x l' r ) &&& Std.Nat.(a < x) &&& inserto a l l';
          (t'' === node x l  l') &&& Std.Nat.(a > x) &&& inserto a r l';
        ])
    ]

(* Top-level wrapper for insertion --- takes and returns non-logic data *)
let insert : int -> Tree.inttree -> Tree.inttree = fun a t ->
  Stream.hd @@
  run q (fun q  -> inserto (Std.nat a) (Tree.inj_tree t) q)
        (fun qs -> qs#reify Tree.prj_exn_tree)

(* Top-level wrapper for "inverse" insertion --- returns an integer, which
   has to be inserted to convert t into t' *)
let uninsert t t' =
  Std.Nat.to_int @@ Stream.hd @@
  run q (fun q  -> inserto q (Tree.inj_tree t) (Tree.inj_tree t'))
        (fun qs -> qs#reify Std.Nat.prj_exn)

(* Entry point *)
let _ =
  let open Printf in
  let insert_list xs =
    let f acc x =
      let acc2 = insert x acc in
      printf "Inserting %d into %s makes %s\n%!" x (Tree.show_inttree acc)
        (Tree.show_inttree acc2);
      acc2
    in
    (* The opening of OCanren hides Stdlib.List *)
    Stdlib.List.fold_left  f Leaf xs
  in
  ignore @@ insert_list [1; 2; 3; 4];
  let t  = insert_list [3; 2; 4; 1] in
  let t' = insert 8 t in
  printf "Inverse insert: %d\n" @@ uninsert t t'
