(*
 * Sort: relational sorting.
 * Copyright (C) 2016
 * Dmitri Boulytchev, 
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
open MiniKanren 

(*
let rec leo x y =
  conde [
    x === !!O;
    fresh (x' y') 
      (x === !!(S x'))
      (y === !!(S y'))
      (leo x' y')
  ]
*)

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max = Nat.(
  conde [
    (min === a) &&& (max === b) &&& (a <= b);
    (max === a) &&& (min === b) &&& (a >  b)
  ]
)

(* [l] is a (non-empty) list, [s] is its smallest element, 
   [l'] --- all other elements
*)
let rec smallesto l s l' =
  conde [       
    (l === !< s) &&& (l' === !!Nil);
    fresh (h t s' t' max)
      (l === h % t)
      (minmaxo h s' s max)
      (l' === max % t')
      (smallesto t s' t')
  ] 

(* Relational sort *)
let rec sorto x y =
  conde [
    (* either both lists are empty *)
    (x === !!Nil) &&& (y === !!Nil);
    fresh (s xs xs')
      (* or the sorted one is a concatenation of the
         smallest (s)element and sorted list of all other elements (xs') *)
      (y === s % xs')
      (smallesto x s xs)
      (sorto xs xs')
  ]

(* Making regular sorting from relational one *)
let sort l =
  run q (sorto @@ inj_nat_list l)
        (fun qs -> prj_nat_list @@ Stream.hd qs)

let perm l = 
  let rec fact = function 0 -> 1 | n -> n * fact (n-1) in
  List.map prj_nat_list @@
  run q (fun q -> sorto q @@ inj_nat_list (List.sort Pervasives.compare l))
        (Stream.take ~n:(fact @@ List.length l))

(* Entry point *)
let _ =

  Printf.printf "%s\n" (show(list) (show(int)) @@ sort [3; 4; 5; 1; 6; 2]);
  Printf.printf "%s\n" (show(list) (show(int)) @@ sort [1; 6; 7; 8; 3; 4; 5]);
  Printf.printf "%s\n" (show(list) (show(int)) @@ sort [2; 3; 6; 7; 8; 3; 6; 7; 2]);


  Printf.printf "%s\n" (show(list) (show(list) (show(int))) @@ perm [1; 2; 3; 4])

(*
  run qr (fun q  r  -> minmaxo q r (inj_nat 4) (inj_nat 3))
         (fun qs rs ->
            Printf.printf "%s\n%s\n"
              (show(list) (show(int)) (List.map prj_nat (Stream.take ~n:1 qs)))
              (show(list) (show(int)) (List.map prj_nat (Stream.take ~n:1 rs)))
         )
*)
(*
  run q (fun q -> leo (inj_nat 2) (inj_nat 3))
        (fun qs ->
           match Stream.take ~n:1 qs with
	   | [] -> Printf.printf "No answer.\n"
	   | _  -> Printf.printf "Some answer(s).\n"
        )

*)
(*
  run qr (fun q  r -> smallesto (inj_nat_list [2; 5; 6; 7]) r q)
         (fun qs rs -> Printf.printf "%s\n" (show(list) (show(int)) @@ prj_nat_list (Stream.hd qs)))
*)
