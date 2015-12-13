open GT
open MiniKanren
open Tester

let g123 x = conde [x === !1; x === !2; x === !3]
let g12  x = (g123 x) &&& (x =/= !3)

let show_int = show(logic) (show int)

let _ = 
  run show_int 3 q (fun q st -> REPR (g123 q st), ["q", q]);
  run show_int 3 q (fun q st -> REPR (g12  q st), ["q", q])

(*
  run show_expr   1 q (fun q   st -> REPR (pExpr (of_list [Id; Mul; Id]) q          st), ["q", q]);  
  run show_expr   1 q (fun q   st -> REPR (pExpr (of_list [Id; Mul; Id; Mul; Id]) q st), ["q", q]);  
  run show_expr   1 q (fun q   st -> REPR (pExpr (of_list [Id; Mul; Id; Add; Id]) q st), ["q", q]);  
  run show_expr   1 q (fun q   st -> REPR (pExpr (of_list [Id; Add; Id; Mul; Id]) q st), ["q", q]);  
  run show_expr   1 q (fun q   st -> REPR (pExpr (of_list [Id; Add; Id; Add; Id]) q st), ["q", q]);  
  run show_stream 1 q (fun q   st -> REPR (pExpr q !(M (!I, !I))                    st), ["q", q])
*)
