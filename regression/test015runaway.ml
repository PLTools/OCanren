open GT
open OCanren
open OCanren.Std
open Tester
open Printf

let ilist xs = list (!!) xs

let runaway_cell: int ilogic List.groundi ref = Stdlib.ref (Obj.magic ())

let demo1 q =
  call_fresh (fun r ->
    runaway_cell := r;
    (q === q)
  )

let demo2 q =
  call_fresh (fun r ->
    (r === !!5) &&&
    conde [ (*(q === nil())
          ; *)(q === !runaway_cell)
          ]
    )

let intlist_reifier =
  List.reify OCanren.reify

let show_int = show(int)
let show_int_list = show(List.ground) show_int
let show2 xs = show(List.logic) (show(logic) show_int) xs

let runT n = run_r intlist_reifier show2 n
let () =
  runT 1 q qh (REPR(demo1));
  let () =
    try runT 2 q qh (REPR(demo2))
    with Failure s -> printf "Failure: \"%s\"\n%!" s
  in
  ()
