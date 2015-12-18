open GT
open MiniKanren
open Tester

let g123 x   = conde [x === !1; x === !2; x === !3]
let g12  x   = (g123 x) &&& (x =/= !3)
let gxy  x y = (g123 x) &&& (g123 y)
let gxy' x y = (gxy x y) &&& (x =/= y)
let gnot5 x  = x =/= !5

let show_int = show(logic) (show int)

let reifier dc x =
  match reify dc x with
  | [] -> ""
  | cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_int x) (show_int c)) cs

let _ = 
  run show_int reifier    3  q (fun q   st -> REPR (g123 q     st), ["q", q]);
  run show_int reifier    3  q (fun q   st -> REPR (g12 q      st), ["q", q]);
  run show_int reifier   10 qr (fun q r st -> REPR (gxy q r    st), ["q", q; "r", r]);
  run show_int reifier   10 qr (fun q r st -> REPR (gxy' q r   st), ["q", q; "r", r]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((q =/= !5) st), ["q", q]); 
  run show_int reifier (-1)  q (fun q   st -> REPR (((q =/= !3) &&& (q === !3)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR (((q === !3) &&& (!3 =/= q)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y) (x === y)(x =/= y)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y) (x =/= y)(x === y)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y) (x =/= y)(!3 === x)(!3 === y)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y) (!3 === x)(x =/= x)(!3 === y)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y) (!3 === x)(!3 === y)(x =/= y)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y) (!3 === x)(!3 === y)(y =/= x)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y z) (x === y)(y === z)(x =/= !4)(z === !(2+2))) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y z) (x === y)(y === z)(z === !(2+2))(x =/= !4)) st), ["q", q]);
  run show_int reifier (-1)  q (fun q   st -> REPR ((fresh (x y z) (x =/= !4)(y === z)(x === y)(z === !(2+2))) st), ["q", q])
