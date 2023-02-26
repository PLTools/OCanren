open OCanren
open OCanren.Std
open Tester

let show_int = GT.show GT.int
let show_intl = GT.show logic (GT.show GT.int)
let run_int eta = run_r OCanren.reify (GT.show logic @@ GT.show GT.int) eta
let show_pairl = GT.show Pair.logic show_intl show_intl
let run_pair eta = run_r (Pair.reify reify reify) show_pairl eta

let trace_int q =
  debug_var q (Fun.flip OCanren.reify) (fun xs ->
    Stdlib.List.iter (fun x -> print_endline @@ show_intl x) xs;
    success)
;;

let trace_pair (q : (_, _) Pair.groundi) =
  debug_var
    q
    (Fun.flip @@ Pair.reify reify reify)
    (fun xs ->
      Stdlib.List.iter (fun x -> print_endline @@ show_pairl x) xs;
      success)
;;

let _ = [%tester run_int (-1) (fun q -> trace_int q)]
let __ _ = [%tester run_int (-1) (fun q -> q === !!1 &&& trace_int q)]
let _ = [%tester run_int (-1) (fun q -> q =/= !!1 &&& trace_int q)]
let _ = [%tester run_pair (-1) (fun q -> q =/= Std.pair !!1 !!2 &&& trace_pair q)]

let _ =
  [%tester
    run_pair (-1) (fun q ->
      fresh (x y) (q =/= Std.pair x y) (x =/= !!1) (y =/= !!2) (trace_pair q))]
;;
