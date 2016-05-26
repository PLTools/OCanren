open GT
open MiniKanren
open Tester

let g123 x   = conde [x === !1; x === !2; x === !3]
let g12  x   = (g123 x) &&& (x =/= !3)
let gxy  x y = (g123 x) &&& (g123 y)
let gxy' x y = (gxy x y) &&& (x =/= y)
let gnot5 x  = x =/= !5

let show_int = show(logic) (show int)

let _ = 
  run show_int    3  q (REPR (fun q   -> g123 q                                                    )) qh;
  run show_int    3  q (REPR (fun q   -> g12 q                                                     )) qh;
  run show_int   10 qr (REPR (fun q r -> gxy q r                                                   )) qrh;
  run show_int   10 qr (REPR (fun q r -> gxy' q r                                                  )) qrh;
  run show_int (-1)  q (REPR (fun q   -> (q =/= !5)                                                )) qh; 
  run show_int (-1)  q (REPR (fun q   -> ((q =/= !3) &&& (q === !3))                               )) qh;
  run show_int (-1)  q (REPR (fun q   -> ((q === !3) &&& (!3 =/= q))                               )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y) (x === y)(x =/= y))                          )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y) (x =/= y)(x === y))                          )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y) (x =/= y)(!3 === x)(!3 === y))               )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y) (!3 === x)(x =/= x)(!3 === y))               )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(x =/= y))               )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(y =/= x))               )) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(x =/= !4)(z === !(2+2))))) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(z === !(2+2))(x =/= !4)))) qh;
  run show_int (-1)  q (REPR (fun q   -> (fresh (x y z) (x =/= !4)(y === z)(x === y)(z === !(2+2))))) qh
