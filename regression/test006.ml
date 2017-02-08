open MiniKanren
open Tester

type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic [@@deriving show { nofullpath = true }]

let rec substo l x a l' =
  conde [
    fresh (y) (l === !(X y))(y === x)(l' === a);
    fresh (m n m' n')
       (l  === !(App (m, n)))
       (l' === !(App (m', n')))
       (substo m x a m')
       (substo n x a n');     
    fresh (v b)
      (l === !(Abs (v, b)))
      (conde [
         (x  === v) &&& (l' === l);
         fresh (b') (l' === !(Abs (v, b'))) (substo b x a b')
       ])    
  ]

let rec evalo m n =
  conde [
    fresh (x) 
      (m === !(X x))
      (n === m);    
    fresh (x l)
      (m === !(Abs (x, l)))
      (n === m);    
    fresh (f a f' a') 
      (m === !(App (f, a)))
      (conde [
         fresh (x l l')     
           (f' === !(Abs (x, l)))
           (substo l x a' l')
           (evalo l' n);         
         fresh (p q) (f' === !(App (p, q))) (n === !(App (f', a')));
         fresh (x) (f' === !(X x)) (n === !(App (f', a')))
       ])
      (evalo f f')
      (evalo a a')
  ]

let show_lam = show_logic (show_lam)

let _ =
  run show_lam 1   q (REPR (fun q   -> substo !(X !"x") !"x" !(X !"y") q                   )) qh;
  run show_lam 1   q (REPR (fun q   -> evalo !(Abs (!"x", !(X !"x"))) q                    )) qh;
  run show_lam 2   q (REPR (fun q   -> evalo !(Abs (!"x", !(X !"x"))) q                    )) qh;
  run show_lam 1   q (REPR (fun q   -> evalo !(App (!(Abs (!"x", !(X !"x"))), !(X !"y"))) q)) qh; 
  run show_lam 1   q (REPR (fun q   -> evalo !(App (!(Abs (!"x", !(X !"x"))), q)) !(X !"y"))) qh;
  run show_lam 1   q (REPR (fun q   -> evalo !(App (!(Abs (!"x", q)), !(X !"y"))) !(X !"y"))) qh;
  run show_lam 1   q (REPR (fun q   -> evalo !(App (q, !(X !"x"))) !(X !"x")               )) qh; 
  run show_lam 1   q (REPR (fun q   -> evalo !(App (!(X !"x"), !(X !"x"))) q               )) qh; 
  run show_lam 1   q (REPR (fun q   -> evalo !(X !"x") q                                   )) qh; 
  run show_lam 1  qr (REPR (fun q r -> evalo !(App (r, q)) !(X !"x")                       )) qrh;
  run show_lam 2 qrs (REPR (fun q r s -> (evalo !(App (q, r)) s) &&& (evalo !(App (r, s)) q) &&& (evalo !(App (s, q)) r))) qrsh


