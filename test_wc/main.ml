open OCanren
open OCanren.Std
open Tester

let show_int = GT.show GT.int
let show_intl = GT.show logic (GT.show GT.int)

let run_bool eta =
  runR OCanren.reify (GT.show GT.bool) (GT.show logic @@ GT.show GT.bool) eta

let run_int eta =
  runR OCanren.reify (GT.show GT.int) (GT.show logic @@ GT.show GT.int) eta

let run_pair eta =
  runR
    (Pair.reify OCanren.reify OCanren.reify)
    (GT.show Pair.ground show_int show_int)
    (GT.show Pair.logic show_intl show_intl)
    eta

let run_list eta =
  runR (List.reify OCanren.reify)
    (GT.show Std.List.ground show_int)
    (GT.show Std.List.logic show_intl)
    eta

let triple a b c = pair a (pair b c)

let run_triple eta =
  runR
    (Pair.reify OCanren.reify (Pair.reify OCanren.reify OCanren.reify))
    (GT.show Pair.ground show_int (GT.show Pair.ground show_int show_int))
    (GT.show Pair.logic show_intl (GT.show Pair.logic show_intl show_intl))
    eta

let _ = [%tester run_int (-1) (fun q -> q === __)]
let _ = [%tester run_int (-1) (fun q -> q =/= __)]
let _ = [%tester run_pair (-1) (fun q -> pair !!2 __ =/= pair __ !!2)]

let _ =
  run_pair (-1) q qh
    (REPR (fun q -> fresh () (q =/= pair __ !!1) (q === pair !!1 __)))

let _ = [%tester run_pair (-1) (fun q -> pair !!1 __ === pair __ !!1)]

let _ =
  [%tester run_pair (-1) (fun q -> q === pair __ !!1 &&& (q === pair !!1 __))]

let _ = [%tester run_pair (-1) (fun q -> pair !!1 __ =/= pair __ !!1)]
let _ = [%tester run_int (-1) (fun q -> triple q !!2 __ =/= triple !!1 __ !!2)]
let _ = [%tester run_int (-1) (fun q r -> pair q r =/= pair !!1 __)]
let _ = [%tester run_int (-1) (fun q -> pair q !!1 =/= pair !!1 __)]
let _ = [%tester run_int (-1) (fun q -> pair q !!1 =/= pair !!1 __)]

let _ =
  [%tester
    run_pair (-1) (fun q ->
        fresh (a b) (q === pair a b) (q =/= pair !!1 __) (q === pair __ !!1) )]

let _ =
  [%tester
    run_pair (-1) (fun q ->
        fresh (a b) (q === pair a b) (q =/= pair !!1 __) (q =/= pair __ !!1) )]

let _ =
  [%tester
    run_pair (-1) (fun q -> fresh () (q =/= pair !!1 __) (q =/= pair __ !!1))]

let _ =
  [%tester
    run_pair (-1) (fun q -> fresh __ (q =/= pair !!1 __) (q =/= pair __ !!1))]

let _ =
  [%tester
    run_pair (-1) (fun q ->
        fresh (a b) (q =/= pair !!1 __) (q === pair __ !!1) (q === pair a b) )]

let _ =
  [%tester
    run_list (-1) (fun q -> fresh () (q === !!1 % (!!2 % __)) (q === __ % __))]

let _ =
  [%tester
    run_list (-1) (fun q -> fresh (a b) (q === !!1 % (!!2 % __)) (q === a % b))]

let _ =
  [%tester
    run_list (-1) (fun q ->
        fresh (a b) (q === __ % __) (q === !<(!!1)) (q === !<(!!2)) )]

let _ = [%tester run_int (-1) (fun q -> q === !!1 &&& (__ =/= !!1 % __))]

let _ =
  [%tester run_int (-1) (fun q -> fresh (a b) (q === !!1) (a =/= !!1 % b))]

let _ = [%tester run_int (-1) (fun q -> fresh a (q === !!1) (a =/= !!1 % a))]
