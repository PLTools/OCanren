open MiniKanren
open Tester

type token = Id | Add | Mul [@@deriving show { nofullpath = true }]
type expr  = I | A of expr logic * expr logic | M of expr logic * expr logic [@@deriving show { nofullpath = true }]

let sym t i i' =
  fresh (x xs) 
    (i === x%xs) (t === x) (i' === xs) 

let eof i = i === !Nil
 
let (|>) x y = fun i i'' r'' ->
  fresh (i' r')
    (x i  i' r')
    (y r' i' i'' r'')

let (<|>) x y = fun i i' r ->
  conde [x i i' r; y i i' r]

let rec pId i i' r = (sym !Id i i') &&& (r === !I)
and pAdd i i' r = (pMulPlusAdd <|> pMul) i i' r
and pMulPlusAdd i i' r = (
      pMul |>
      (fun r i i'' r'' ->
         fresh (r' i') 
           (sym !Add i i')
           (r'' === !(A (r, r')))
           (pAdd i' i'' r')       
      )) i i' r      
and pMul i i' r = (pIdAstMul <|> pId) i i' r 
and pIdAstMul i i' r= (
      pId |>
      (fun r i i'' r'' -> 
         fresh (r' i') 
           (sym !Mul i i')
           (r'' === !(M (r, r'))) 
           (pMul i' i'' r')
      )) i i' r
and pTop i i' r = pAdd i i' r

let pExpr i r = fresh (i') (pTop i i' r) (eof i')

let show_token' = show_token

let show_token  = show_logic show_token
let show_expr   = show_logic show_expr
let show_stream = List.show_logic (show_logic show_token')

let _ =
  run show_expr   1 q (REPR (fun q -> pExpr (inj_list [Id]) q                  )) qh;
  run show_expr   1 q (REPR (fun q -> pExpr (inj_list [Id; Mul; Id]) q         )) qh;  
  run show_expr   1 q (REPR (fun q -> pExpr (inj_list [Id; Mul; Id; Mul; Id]) q)) qh;  
  run show_expr   1 q (REPR (fun q -> pExpr (inj_list [Id; Mul; Id; Add; Id]) q)) qh;  
  run show_expr   1 q (REPR (fun q -> pExpr (inj_list [Id; Add; Id; Mul; Id]) q)) qh;  
  run show_expr   1 q (REPR (fun q -> pExpr (inj_list [Id; Add; Id; Add; Id]) q)) qh;  
  run show_stream 1 q (REPR (fun q -> pExpr q !(M (!I, !I))                   )) qh
