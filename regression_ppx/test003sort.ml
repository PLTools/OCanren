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

(* This is reimplementation of samples/sorting.ml with camlp5 but with PPX *)

open GT
open OCanren
open OCanren.Std

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max =
  let open Nat in
  conde
    [ min === a &&& (max === b) &&& leo a b !!true
    ; min === b &&& (max === a) &&& gto a b !!true
    ]
;;

(* [l] is a (non-empty) list, [s] is its smallest element,
   [l'] --- all other elements
*)
let rec smallesto l s l' =
  conde
    [ l === !<s &&& (l' === List.nil ())
    ; fresh
        (h t s' t' max)
        (l' === max % t')
        (l === h % t)
        (minmaxo h s' s max)
        (smallesto t s' t')
    ]
;;

(* Relational sort *)
let rec sorto x y =
  conde
    [ (* either both lists are empty *)
      x === nil () &&& (y === x)
    ; (* or the sorted one is a concatenation of the
      smallest element (s) and sorted list of all other elements (xs')
   *)
      fresh (s xs xs') (y === s % xs') (sorto xs xs') (smallesto x s xs)
    ]
;;

(* Some shortcuts to make regular lists from relational ones *)
let int_list = Stdlib.List.map Nat.to_int

let (_ : (Nat.groundi List.groundi, Nat.ground List.ground) Reifier.t) =
  Std.List.prj_exn Nat.prj_exn
;;

let project
  : ((Nat.groundi as 'a), 'a List.groundi) List.t reified -> Nat.ground List.ground
  =
 fun rr -> rr#reify (List.prj_exn Nat.prj_exn)
;;

(* Making regular sorting from relational one *)
let sort l = int_list @@ Stream.hd @@ run q (sorto @@ nat_list l) project

(* A straightforward implementation of factorial *)
let rec fact = function
  | 0 -> 1
  | n -> n * fact (n - 1)
;;

(* Making permutations from relational sorting *)
let perm l =
  Stdlib.List.map int_list
  @@ Stream.take ~n:(fact @@ Stdlib.List.length l)
  @@ run q (fun q -> sorto q @@ nat_list (Stdlib.List.sort Stdlib.compare l)) project
;;

(* More hardcore version: no standard sorting required *)
let perm' l =
  Stdlib.List.map int_list
  @@ Stream.take ~n:(fact @@ Stdlib.List.length l)
  @@ run q (fun q -> fresh r (sorto (nat_list l) r) (sorto q r)) project
;;

(* Some auxilliary type shortcuts *)
type il = GT.int GT.list [@@deriving gt ~options:{ show }]
type ill = GT.int GT.list GT.list [@@deriving gt ~options:{ show }]

(* Entry point *)
let _ =
  (* Sorting: *)
  Printf.printf "%s\n\n%!" (show il @@ sort []);
  Printf.printf "%s\n\n%!" (show il @@ sort [ 1 ]);
  Printf.printf "%s\n\n%!" (show il @@ sort [ 2; 1 ]);
  Printf.printf "%s\n\n%!" (show il @@ sort [ 3; 2; 1 ]);
  Printf.printf "%s\n\n%!" (show il @@ sort [ 4; 3; 2; 1 ]);
  (* Permutations: *)
  Printf.printf "%s\n\n%!" (show ill @@ perm []);
  Printf.printf "%s\n\n%!" (show ill @@ perm [ 1 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm [ 1; 2 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm [ 1; 2; 3 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm [ 1; 2; 3; 4 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm [ 1; 2; 3; 4; 5 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm [ 1; 2; 3; 4; 5; 6 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm' []);
  Printf.printf "%s\n\n%!" (show ill @@ perm' [ 1 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm' [ 1; 2 ]);
  Printf.printf "%s\n\n%!" (show ill @@ perm' [ 1; 2; 3 ])
;;
