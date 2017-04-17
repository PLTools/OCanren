(*
 * Sort: relational sorting.
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

open GT
open Printf
open MiniKanren
open Tester

let show_nat_list = GT.(show List.ground @@ show Nat.ground)
let show_nat      = GT.(show Nat.ground)

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max =
  let open Nat in
  conde
    [ (min === a) &&& (max === b) &&& (a <= b)
    ; (max === a) &&& (min === b) &&& (a >  b)
    ]

(* [l] is a (non-empty) list, [s] is its smallest element,
   [l'] --- all other elements
*)
let rec smallesto l s l' = conde
  [ (l === !< s) &&& (l' === nil())
  ; fresh (h t s' t' max)
      (l' === max % t')
      (l === h % t)
      (minmaxo h s' s max)
      (smallesto t s' t')
  ]

(* Relational sort *)
let rec sorto x y =
  conde
    [ (* either both lists are empty *)
      (x === nil()) &&& (y === nil())
    ; fresh (s xs xs')
      (* or the sorted one is a concatenation of the
        smallest element (s) and sorted list of all other elements (xs')
      *)
        (y === s % xs')
        (sorto xs xs')       (* 1 *)
        (smallesto x s xs)   (* 2 *)
    ]

let _ =
  run four  (fun q1 q2 q3 p -> sorto (q1 % (q2 % (q3 % nil ()))) p)
            (fun _  _  _  p ->
              Stream.take ~n:10 p |> List.iter (fun rr ->
                printf "%s\n%!"  @@ (if rr#is_open
                then
                  GT.(show List.logic (show Nat.logic)) @@
                    rr#refine (List.reify Nat.reify) ~inj:(List.to_logic Nat.to_logic)
                else
                  GT.(show List.ground (show Nat.ground) rr#prj)
                )
              )
            )

(* Making regular sorting from relational one *)
let sort l =
  List.to_list Nat.to_int @@
  run q (sorto @@ inj_nat_list l)
        (fun qs -> Stream.hd qs |> (fun rr -> rr#prj))

(* Veeeeery straightforward implementation of factorial *)
let rec fact = function 0 -> 1 | n -> n * fact (n-1)


(* Making permutations from relational sorting *)
let perm l =
  List.map (List.to_list Nat.to_int) @@
  run q (fun q -> sorto q @@ inj_nat_list (List.sort Pervasives.compare l))
        (fun qs ->
          qs |> Stream.take ~n:(fact @@ List.length l) |>
          List.map (fun rr -> rr#prj))

(* More hardcore version: no standard sorting required *)
let perm' l =
  List.map (List.to_list Nat.to_int) @@
  run q (fun q -> fresh (r) (sorto (inj_nat_list l) r) (sorto q r))
        (fun qs ->
          qs |> Stream.take ~n:(fact @@ List.length l)
          |> List.map (fun rr -> rr#prj))

(* Entry point *)
let _ =
  (* Sorting: *)

  Printf.printf "%s\n\n%!" (show(list) (show(int)) @@ sort []);
  Printf.printf "%s\n\n%!" (show(list) (show(int)) @@ sort [1]);
  Printf.printf "%s\n\n%!" (show(list) (show(int)) @@ sort [2; 1]);
  Printf.printf "%s\n\n%!" (show(list) (show(int)) @@ sort [3; 2; 1]);
  Printf.printf "%s\n\n%!" (show(list) (show(int)) @@ sort [4; 3; 2; 1]);

  (* Alas, this one is too slow:

       Printf.printf "%s\n\n%!" (show(list) (show(int)) @@ sort [7; 4; 3; 2; 1]);
     To make it run faster, lines (* 1 *) and (* 2 *) in ``sorto'' implementation
     has to be switched; then, naturally, permutations stop to work.

     The following (somewhat shameful) implementation, however, works for both cases:
     let rec sorto x y = conde [
       (x === !!Nil) &&& (y === !!Nil);
       fresh (s xs xs')
         (y === s % xs')
         (smallesto x s xs)
         (sorto xs xs');
       fresh (s xs xs')
         (y === s % xs')
         (sorto xs xs')
         (smallesto x s xs)
     ]
  *)

  (* Permutations: *)

  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm []);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1; 2]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1; 2; 3]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1; 2; 3; 4]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1; 2; 3; 4; 5]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1; 2; 3; 4; 5; 6]);

  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm' []);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm' [1]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm' [1; 2]);
  Printf.printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm' [1; 2; 3])
