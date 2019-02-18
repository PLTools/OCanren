open MiniKanren
open Tester
open GT
open MiniKanrenStd

let o = Nat.zero
let l = Nat.succ o

let rec test = Tabling.(tabled one) @@
   fun x -> conde [x === o; x === l]

let run = runR (List.reify Nat.reify) (show List.ground (show Nat.ground)) (show List.logic (show Nat.logic))

let () =
  run (-1) q qh ("test", fun q ->
    fresh (x y z)
      (q === x % (y % nil ()))
      (test x)
      (test y));
