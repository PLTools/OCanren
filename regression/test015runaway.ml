open MiniKanren
open Tester
open Printf

let ilist xs = inj_list @@ List.map inj_int xs

let runaway_cell: (int List.ground, int logic List.logic) fancy ref = ref (Obj.magic ())

let demo1 q =
  call_fresh (fun r ->
    runaway_cell := r;
    (q === q)
  )

let demo2 q =
  call_fresh (fun r ->
    (r === inj_int 5) &&&
    conde [ (*(q === nil())
          ; *)(q === !runaway_cell)
          ]
    )

let intlist_reifier c xs =
  List.reifier ManualReifiers.int_reifier c xs

let show_int = string_of_int
let show_int_list = GT.show(List.ground) show_int
let show2 xs = GT.(show List.logic @@ show_logic show_int) xs

let runT n = runR intlist_reifier show_int_list show2 n
let () =
  runT 1 q qh (REPR(demo1));
  let () =
    try runT 2 q qh (REPR(demo2))
    with Failure s -> printf "Failure: \"%s\"\n%!" s
  in
  ()
