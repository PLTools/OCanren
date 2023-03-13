(*
 * Sort: relational sorting.
 * Copyright (C) 2016-2023
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

let _ : (Nat.groundi List.groundi, Nat.logic List.logic) Reifier.t =
  List.reify Nat.reify
let _ : (Nat.groundi List.groundi, Nat.ground List.ground) Reifier.t =
  List.prj_exn Nat.prj_exn

let _ = Stream.take ~n:10 @@
  run qrst  (fun q1 q2 q3 p -> sorto (q1 % (q2 % (q3 % nil ()))) p)
            (fun _  _  _ rr ->
              printf "%s\n%!"  @@ (if rr#is_open
              then
                GT.(show List.logic (show Nat.logic)) @@
                  rr#reify (List.reify Nat.reify)
              else
                GT.(show List.ground (show Nat.ground)) @@
                  rr#reify (List.prj_exn  Nat.prj_exn)
              )
            )

(* Making regular sorting from relational one *)
let sort l =
  Stdlib.List.map Nat.to_int @@
  Stream.hd @@
    run q (sorto @@ nat_list l)
          (fun rr -> rr#reify (List.prj_exn Nat.prj_exn) )

(* Veeeeery straightforward implementation of factorial *)
let rec fact = function 0 -> 1 | n -> n * fact (n-1)


(* Making permutations from relational sorting *)
let perm l =
  L.map (Stdlib.List.map Nat.to_int) @@
  Stream.take ~n:(fact @@ L.length l) @@
    run q (fun q -> sorto q @@ nat_list (L.sort Stdlib.compare l))
          (fun rr -> rr#reify (List.prj_exn Nat.prj_exn))

(* More hardcore version: no standard sorting required *)
let perm' l =
  L.map (Stdlib.List.map Nat.to_int) @@
  Stream.take ~n:(fact @@ L.length l) @@
    run q (fun q -> fresh (r) (sorto (nat_list l) r) (sorto q r))
          (fun rr -> rr#reify (List.prj_exn  Nat.prj_exn ))

(* Entry point *)
let _ =
  (* Sorting: *)

  Printf.printf "%s\n\n%!" (show(GT.list) (show(int)) @@ sort []);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(int)) @@ sort [1]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(int)) @@ sort [2; 1]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(int)) @@ sort [3; 2; 1]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(int)) @@ sort [4; 3; 2; 1]);

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

  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm []);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm [1]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm [1; 2]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm [1; 2; 3]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm [1; 2; 3; 4]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm [1; 2; 3; 4; 5]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm [1; 2; 3; 4; 5; 6]);

  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm' []);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm' [1]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm' [1; 2]);
  Printf.printf "%s\n\n%!" (show(GT.list) (show(GT.list) (show(int))) @@ perm' [1; 2; 3])
