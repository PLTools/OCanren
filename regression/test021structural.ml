open Printf
open OCanren
open OCanren.Std
open Tester

(* all lists that contain ones *)
let rec ones out =
  conde
    [ List.nullo out
    ; fresh (d)
        ((!!1 % d) === out)
        (ones d)
  ]


let show_int_list   = GT.(show List.ground @@ show int)
let show_intl = GT.(show logic @@ show int)
let show_intl_llist = GT.(show List.logic show_intl)

let reifier = List.reify OCanren.reify
let runL n = run_r reifier show_intl_llist n

let even xs =
  (* returns false when constraint is violated *)
  let rec helper ~iseven = function
  | Var (_,_) -> true  (* list can be any length *)
  | Value (Std.List.Cons (_, tl)) -> helper ~iseven:(not iseven) tl
  | Value Nil -> iseven
  in
  helper ~iseven:true xs

let list_of_ones_even_length q = (structural q reifier even) &&& (ones q)

let _freeVars =
  runL  10  q  qh (REPR(list_of_ones_even_length))

let run_int n = run_r OCanren.reify show_intl n
let _noAnswers =
  let remove_answer_by_constraint q =
    fresh (dummy)
      (q === !!5)
      (structural q OCanren.reify (fun _ -> false))
  in
  run_int (-1) q qh (REPR(remove_answer_by_constraint))
