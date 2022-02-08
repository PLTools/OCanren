open OCanren
open OCanren.Std
open Tester
open GT

let o = Nat.zero
let l = Nat.succ o

let rec test = Tabling.(tabled one) @@
   fun x -> conde [x === o; x === l]

let run n = run_r (List.reify Nat.reify) (show List.logic (show Nat.logic)) n

let () =
  run (-1) q qh ("test", fun q ->
    fresh (x y z)
      (q === x % (y % nil ()))
      (test x)
      (test y));
