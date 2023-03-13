(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * Sort: relational sorting.
 * Copyright (C) 2016-2022
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

open GT
open Printf

module L = List

open OCanren
open OCanren.Std

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max =
  let open Nat in
  ocanren {
    min == a & max == b & a <= b |
    min == b & max == a & a >  b
  }

(* [l] is a (non-empty) list, [s] is its smallest element,
   [l'] --- all other elements
*)
let rec smallesto l s l' =
  ocanren {
    l == [s] & l' == [] |
    fresh h, t, s', t', max in
      l' == max :: t' &
      l  == h :: t &
      minmaxo h s' s max &
      smallesto t s' t'
  }

(* Relational sort *)
let rec sorto x y =
  ocanren {
   (* either both lists are empty *)
    x == [] & y == [] |
   (* or the sorted one is a concatenation of the
      smallest element (s) and sorted list of all other elements (xs')
   *)
   fresh s, xs, xs' in
     y == s :: xs' &
     sorto xs xs'  &
     smallesto x s xs
  }

(* Some shortcuts to make regular lists from relational ones *)
let int_list = Stdlib.List.map Nat.to_int

(* Making regular sorting from relational one *)
let sort l = int_list @@ Stream.hd @@ run q (sorto @@ nat_list l) (fun r -> r#reify (Std.List.prj_exn Nat.prj_exn))

(* A straightforward implementation of factorial *)
let rec fact = function 0 -> 1 | n -> n * fact (n-1)

(* Making permutations from relational sorting *)
let perm l =
  L.map int_list @@
    Stream.take ~n:(fact @@ L.length l) @@
      run q (fun q -> sorto q @@ nat_list (L.sort Stdlib.compare l)) (fun r -> r#reify (Std.List.prj_exn Nat.prj_exn))

(* More hardcore version: no standard sorting required *)
let perm' l =
  L.map int_list @@
    Stream.take ~n:(fact @@ L.length l) @@
      run q (fun q -> fresh (r) (sorto (nat_list l) r) (sorto q r)) (fun r -> r#reify (Std.List.prj_exn Nat.prj_exn));;

(* Some auxilliary type shortcuts *)
@type il  = GT.int GT.list         with show
@type ill = GT.int GT.list GT.list with show

(* Entry point *)
let _ =
  (* Sorting: *)
  Printf.printf "%s\n\n%!" (show(il) @@ sort []);
  Printf.printf "%s\n\n%!" (show(il) @@ sort [1]);
  Printf.printf "%s\n\n%!" (show(il) @@ sort [2; 1]);
  Printf.printf "%s\n\n%!" (show(il) @@ sort [3; 2; 1]);
  Printf.printf "%s\n\n%!" (show(il) @@ sort [4; 3; 2; 1]);

  (* Permutations: *)
  Printf.printf "%s\n\n%!" (show(ill) @@ perm []);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm [1]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm [1; 2]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm [1; 2; 3]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm [1; 2; 3; 4]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm [1; 2; 3; 4; 5]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm [1; 2; 3; 4; 5; 6]);

  Printf.printf "%s\n\n%!" (show(ill) @@ perm' []);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm' [1]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm' [1; 2]);
  Printf.printf "%s\n\n%!" (show(ill) @@ perm' [1; 2; 3])
