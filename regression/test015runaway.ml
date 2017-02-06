open MiniKanren
open Tester
open Printf

let inj_int n : (int,int) fancy = inj@@lift n
let ilist xs = inj_list @@ List.map inj_int xs

(* reifier *)
let ilist_of_ftyp (c: var_checker) f =
  let rec helper (t: (int,int) List.flist) : int logic List.logic =
    if c#isVar t then refine_fancy t c helper
    else match coerce_fancy t with
    | Nil -> Value Nil
    | Cons (h, tl) when c#isVar h -> Value (Cons (refine_fancy h c (intl_of_intf c), helper tl))
    | Cons (h, tl) -> Value (Cons (Value (coerce_fancy h), helper tl))
  in
  helper f

let runaway_cell: (int,int) List.flist ref = ref (Obj.magic ())

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


let show_int = string_of_int
let show_int_logic = show_logic show_int
let show_int_list : int list -> string = GT.show(GT.list) show_int
let show2 xs = (GT.show List.logic) (GT.show(logic) (GT.show GT.int)) xs

let runT n = runR ilist_of_ftyp show_int_list show2 n
let _ =
  runT 1 q (REPR(demo1)) qh;
  let () =
    try runT 2 q (REPR(demo2)) qh
    with Failure s -> printf "Failure: \"%s\"\n%!" s
  in
  ()
(*
let _withFree () =
  run_exn show_int_list  1  q (REPR (fun q   -> reverso (ilist []) (ilist [])                )) qh;
  run_exn show_int_list  4 qr (REPR (fun q r -> appendo q (ilist []) r                          )) qrh;
  run_exn show_int_list  1  q (REPR (fun q   -> reverso q q                                        )) qh;
  run_exn show_int_list  2  q (REPR (fun q   -> reverso q q                                        )) qh;
  run_exn show_int_list  3  q (REPR (fun q   -> reverso q q                                        )) qh;
  run_exn show_int_list 10  q (REPR (fun q   -> reverso q q                                        )) qh;
  () *)
