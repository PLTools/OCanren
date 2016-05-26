open GT
open MiniKanren
open Tester

@type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic with show

let match_lam a var app abs =
  conde [
    fresh (x)   (a === !(X x))        (var x); 
    fresh (p q) (a === !(App (p, q))) (app p q);
    fresh (x l) (a === !(Abs (x, l))) (abs x l) 
  ]

let rec substo l x a l' =  
  match_lam l
    (fun y   -> (x === y) &&& (l' === a)) 
    (fun p q -> fresh (p' q') 
                  (l' === !(App (p', q'))) 
                  (substo p x a p') 
                  (substo q x a q')
    )
    (fun v b -> conde [(x === v) &&& (l' === l); 
                       fresh (b') 
                         (l' === !(Abs (v, b'))) 
                         (substo b x a b')
                      ]) 

let rec evalo m n =
  match_lam m
    (fun _ -> n === m)
    (fun f a -> 
       fresh (f' a') 
         (match_lam f' 
            (fun _   -> n === !(App (f', a')))
            (fun _ _ -> n === !(App (f', a')))
            (fun x l -> fresh (l') 
                          (substo l x a' l') 
                          (evalo l' n))
         )
         (evalo f f')
         (evalo a a')
    )
    (fun _ _ -> n === m)

let show_lam = show logic (show lam)

let _ =
  run show_lam 1  q (REPR (fun q   -> substo !(X !"x") !"x" !(X !"y") q                   )) qh;
  run show_lam 1  q (REPR (fun q   -> evalo !(Abs (!"x", !(X !"x"))) q                    )) qh;
  run show_lam 2  q (REPR (fun q   -> evalo !(Abs (!"x", !(X !"x"))) q                    )) qh;
  run show_lam 1  q (REPR (fun q   -> evalo !(App (!(Abs (!"x", !(X !"x"))), !(X !"y"))) q)) qh; 
  run show_lam 1  q (REPR (fun q   -> evalo !(App (!(Abs (!"x", !(X !"x"))), q)) !(X !"y"))) qh;
  run show_lam 1  q (REPR (fun q   -> evalo !(App (!(Abs (!"x", q)), !(X !"y"))) !(X !"y"))) qh;
  run show_lam 1  q (REPR (fun q   -> evalo !(App (q, !(X !"x"))) !(X !"x")               )) qh; 
  run show_lam 1  q (REPR (fun q   -> evalo !(App (!(X !"x"), !(X !"x"))) q               )) qh; 
  run show_lam 1  q (REPR (fun q   -> evalo !(X !"x") q                                   )) qh; 
  run show_lam 1 qr (REPR (fun q r -> evalo !(App (r, q)) !(X !"x")                       )) qrh
