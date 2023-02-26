(* Wildcard variables allows us to express "a peano number that is less than a constant" *)

open OCanren
open Tester

let run_nat eta = run_r Std.Nat.reify (GT.show Std.Nat.logic) eta

let of_int =
  let rec helper acc n = if n > 0 then helper (Std.Nat.succ acc) (n - 1) else acc in
  helper Std.Nat.zero
;;

(* q >= 2 *)
let _ =
  let open Std.Nat in
  [%tester run_nat (-1) (fun q -> fresh v (q === succ (succ v)))]
;;

(* q <= 3 *)
let le3 q =
  let open Std.Nat in
  q =/= succ (succ (succ __))
;;

(* 2 <= 3 *)
let _ = [%tester run_nat (-1) (fun q -> fresh () (le3 q) (q === of_int 2))]

(* 3 <= 3 *)
let _ = [%tester run_nat (-1) (fun q -> fresh () (le3 q) (q === of_int 3))]

(* 1 <= 3 *)
let _ = [%tester run_nat (-1) (fun q -> fresh () (le3 q) (q === of_int 1))]

(* 0 <= 3 *)
let _ = [%tester run_nat (-1) (fun q -> fresh () (le3 q) (q === of_int 0))]

(* not 4 <= 3 *)
let _ = [%tester run_nat (-1) (fun q -> fresh () (le3 q) (q === of_int 4))]
