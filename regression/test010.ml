(* Some tests about constraints *)
open GT
open MiniKanren
open Std
open Tester

let (!) = (!!)
let g123 x   = conde [x === !1; x === !2; x === !3]
let g12  x   = (g123 x) &&& (x =/= !3)
let gxy  x y = (g123 x) &&& (g123 y)
let gxy' x y = (gxy x y) &&& (x =/= y)
let gnot5 x  = x =/= !5

let show_int = show(int)
let show_fint = show(int)

let _ =
  run_exn show_fint    3    q   qh (REPR (fun q   -> g123 q                                                    ));
  run_exn show_fint    3    q   qh (REPR (fun q   -> g12 q                                                     ));
  run_exn show_fint   10   qr  qrh (REPR (fun q r -> gxy q r                                                   ));
  run_exn show_fint   10   qr  qrh (REPR (fun q r -> gxy' q r                                                  ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y) (x === y)(x =/= y))                          ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y) (x =/= y)(x === y))                          ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y) (x =/= y)(!3 === x)(!3 === y))               ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y) (!3 === x)(x =/= x)(!3 === y))               ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(x =/= y))               ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(y =/= x))               ));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(x =/= !4)(z === !(2+2)))));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(z === !(2+2))(x =/= !4))));
  run_exn show_fint (-1)    q   qh (REPR (fun q   -> (fresh (x y z) (x =/= !4)(y === z)(x === y)(z === !(2+2)))))

let runI n = runR MiniKanren.reify show_int (show(logic) show_int) n

let _ =
  runI (-1)  q qh (REPR (fun q   -> (q =/= !5)                                                ));
  runI (-1)  q qh (REPR (fun q   -> ((q =/= !3) &&& (q === !3))                               ));
  runI (-1)  q qh (REPR (fun q   -> ((q === !3) &&& (!3 =/= q))                               ))

let show_bool = show(bool)

let runB n = runR MiniKanren.reify show_bool (show(logic) show_bool) n

let _ =
  runB (-1) qr qrh (REPR (fun q r -> (q =/= (!!true)) &&& (q =/= r)))
