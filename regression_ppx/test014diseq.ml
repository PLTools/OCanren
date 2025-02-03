(* A motivating example for disequality constraints.
   W.Byrd Ph.D., chapter 7 *)

let () = print_endline "test014 rembero"

open OCanren

let rec rembero1 x xs out =
  conde
    [ xs === Std.nil () &&& (xs === out)
    ; fresh (h tl) (xs === Std.List.cons h tl) (h === x) (tl === out)
    ; fresh (h tl res) (xs === Std.List.cons h tl) (out === Std.List.cons h res) (rembero1 x tl res)
    ]
;;

let rec rembero2 x xs out =
  conde
    [ xs === Std.nil () &&& (xs === out)
    ; fresh (h tl) (xs === Std.List.cons h tl) (h === x) (tl === out)
    ; fresh
        (h tl res)
        (xs === Std.List.cons h tl)
        (h =/= x)
        (out === Std.List.cons h res)
        (rembero2 x tl res)
    ]
;;

open Tester

let () =
  run_r
    [%reify: GT.int Std.List.ground]
    ([%show: GT.int OCanren.logic Std.List.logic] ())
    4
    q
    qh
    ("bad rembero", rembero1 !!2 (Std.list ( !! ) [ 1; 2; 3; 2; 4 ]))
;;

let () =
  run_r
    [%reify: GT.int Std.List.ground]
    ([%show: GT.int OCanren.logic Std.List.logic] ())
    4
    q
    qh
    ("good rembero", rembero2 !!2 (Std.list ( !! ) [ 1; 2; 3; 2; 4 ]))
;;
