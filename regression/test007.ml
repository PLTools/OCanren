1open GT
open MiniKanren
open Tester

@type lam = X of string | App of lam * lam | Abs of string * lam with mkshow

let match_lam a var (*app abs*) =
  conde [
    fresh (x)   (a === X x)        (var x); (*
    fresh (p q) (a === App (p, q)) (app p q);
    fresh (x l) (a === Abs (x, l)) (abs x l) *)
  ]
(*
let rec substo l x a l' =  
  match_lam l
    (fun y   -> conde [x === y &&& l' === a; l' === l]) (*
    (fun p q -> fresh (p' q') (l' === App (p', q')) (substo p x a p') (substo q x a q'))
    (fun v b -> conde [x === v &&& l' === l; fresh (b') (l' === Abs (v, b')) (substo b x a b')]) *)
*)

let rec substo l x a l' =
  conde [
    fresh (y) (l === X y)(y === x)(l' === a);
    fresh (m n m' n')
       (l  === App (m, n))
       (l' === App (m', n'))
       (substo m x a m')
       (substo n x a n');     
    fresh (v b)
      (l === Abs (v, b))
      (conde [
         (x  === v) &&& (l' === l);
         fresh (b') (l' === Abs (v, b')) (substo b x a b')
       ])    
  ]

let rec evalo m n =
  conde [
    fresh (x) 
      (m === X x)
      (n === m);    
    fresh (x l)
      (m === Abs (x, l))
      (n === m);    
    fresh (f a f' a') 
      (m === App (f, a))
      (conde [
         fresh (x l l')     
           (f' === Abs (x, l))
           (substo l x a' l')
           (evalo l' n);         
         fresh (p q) (f' === App (p, q)) (n === App (f', a'));
         fresh (x) (f' === X x) (n === App (f', a'))
       ])
      (evalo f f')
      (evalo a a')
  ]

let _ =
  run (mkshow lam)    1  q (fun q   st -> REPR (substo (X "x") "x" (X "y") q st)             , ["q", q]);
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (Abs ("x", X "x")) q st)               , ["q", q]);
  run (mkshow lam)    2  q (fun q   st -> REPR (evalo (Abs ("x", X "x")) q st)               , ["q", q]);
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (App (Abs ("x", X "x"), X "y")) q st)  , ["q", q]);
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (App (Abs ("x", X "x"), q)) (X "y") st), ["q", q]);
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (App (Abs ("x", q), X "y")) (X "y") st), ["q", q]);
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (App (q, X "x")) (X "x") st)           , ["q", q]); 
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (App (X "x", X "x")) q st)             , ["q", q]); 
  run (mkshow lam)    1  q (fun q   st -> REPR (evalo (X "x") q st)                          , ["q", q]); 
  run (mkshow lam)    1 qp (fun q p st -> REPR (evalo (App (p, q)) (X "x") st)               , ["q", q; "p", p])
