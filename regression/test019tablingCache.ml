open MiniKanren
open MiniKanren.Std
open Tester
open GT

let o = LNat.zero
let l = LNat.succ o

let rec test = Tabling.(tabled one) @@
   fun x -> conde [x === o; x === l]

let run = runR (LList.reify LNat.reify) (show LList.ground (show LNat.ground)) (show LList.logic (show LNat.logic))

let () =
  run (-1) q qh ("test", fun q ->
    fresh (x y z)
      (q === x % (y % nil ()))
      (test x)
      (test y));
