open GT
open MiniKanren
open Tester

@type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic with show

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

let show_lam = show logic (show lam)

let _ =
  run show_lam empty_reifier 1  q (fun q   st -> REPR (substo !(X !"x") !"x" !(X !"y") q                    st), ["q", q]);
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(Abs (!"x", !(X !"x"))) q                     st), ["q", q]);
  run show_lam empty_reifier 2  q (fun q   st -> REPR (evalo !(Abs (!"x", !(X !"x"))) q                     st), ["q", q]);
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(App (!(Abs (!"x", !(X !"x"))), !(X !"y"))) q st), ["q", q]); 
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(App (!(Abs (!"x", !(X !"x"))), q)) !(X !"y") st), ["q", q]);
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(App (!(Abs (!"x", q)), !(X !"y"))) !(X !"y") st), ["q", q]);
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(App (q, !(X !"x"))) !(X !"x")                st), ["q", q]); 
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(App (!(X !"x"), !(X !"x"))) q                st), ["q", q]); 
  run show_lam empty_reifier 1  q (fun q   st -> REPR (evalo !(X !"x") q                                    st), ["q", q]); 
  run show_lam empty_reifier 1 qr (fun q r st -> REPR (evalo !(App (r, q)) !(X !"x")                        st), ["q", q; "r", r])



