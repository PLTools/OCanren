open GT
open MiniKanren
open Tester

let (!) = fun x -> inj@@lift x
let g123 x   = conde [x === !1; x === !2; x === !3]
let g12  x   = (g123 x) &&& (x =/= !3)
let gxy  x y = (g123 x) &&& (g123 y)
let gxy' x y = (gxy x y) &&& (x =/= y)
let gnot5 x  = x =/= !5

let show_int = GT.(show int)
(* let show_fint = show_fancy (show int) *)

let _ =
  run_exn show_int    3  q (REPR (fun q   -> g123 q                                                    )) qh;
  run_exn show_int    3  q (REPR (fun q   -> g12 q                                                     )) qh;
  run_exn show_int   10 qr (REPR (fun q r -> gxy q r                                                   )) qrh;
  run_exn show_int   10 qr (REPR (fun q r -> gxy' q r                                                  )) qrh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y) (x === y)(x =/= y))                          )) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y) (x =/= y)(x === y))                          )) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y) (x =/= y)(!3 === x)(!3 === y))               )) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y) (!3 === x)(x =/= x)(!3 === y))               )) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(x =/= y))               )) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(y =/= x))               )) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(x =/= !4)(z === !(2+2))))) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(z === !(2+2))(x =/= !4)))) qh;
  run_exn show_int (-1)  q (REPR (fun q   -> (fresh (x y z) (x =/= !4)(y === z)(x === y)(z === !(2+2))))) qh;
  ()


let intl_of_intf cond y : int logic =
  let rec helper y =
    if cond @@ Obj.repr y
    then refine_fancy3 y cond helper
    else Value (coerce_fancy y)
  in
  helper y

let runI n = runR intl_of_intf show_int (show_logic string_of_int) n

let _ =
  runI (-1)  q (REPR (fun q   -> (q =/= !5)                                                )) qh;
  runI (-1)  q (REPR (fun q   -> ((q =/= !3) &&& (q === !3))                               )) qh;
  runI (-1)  q (REPR (fun q   -> ((q === !3) &&& (!3 =/= q))                               )) qh;
  ()
