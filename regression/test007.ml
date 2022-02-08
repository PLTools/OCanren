open GT
open OCanren
open Tester
open Stlc
open GLam

let match_lam a onVar onApp onAbs =
  conde [
    fresh (x)   (a === (v x))     (onVar x);
    fresh (p q) (a === (app p q)) (onApp p q);
    fresh (x l) (a === (abs x l)) (onAbs x l);
  ]

let rec substo l x a l' =
  match_lam l
    (fun y   -> (x === y) &&& (l' === a))
    (fun p q -> fresh (p' q')
                  (l' === (app p' q'))
                  (substo p x a p')
                  (substo q x a q')
    )
    (fun v b -> conde [(x === v) &&& (l' === l);
                       fresh (b')
                         (l' === (abs v b'))
                         (substo b x a b')
                      ])

let rec evalo m n =
  match_lam m
    (fun _ -> n === m)
    (fun f a ->
       fresh (f' a')
         (match_lam f'
            (fun _   -> n === (app f' a'))
            (fun _ _ -> n === (app f' a'))
            (fun x l -> fresh (l')
                          (substo l x a' l')
                          (evalo l' n))
         )
         (evalo f f')
         (evalo a a')
    )
    (fun _ _ -> n === m)

let run_lam eta = run_r GLam.prj_exn GLam.show_rlam eta
let _ =
  run_lam 1    q   qh (REPR (fun q   -> substo (v varX) varX (v varY) q                   ));
  run_lam 2    q   qh (REPR (fun q   -> evalo (abs varX (v varX)) q                       ));
  run_lam 1    q   qh (REPR (fun q   -> evalo (abs varX (v varX)) q                       ));
  run_lam 1    q   qh (REPR (fun q   -> evalo (app (abs varX (v varX)) (v varY)) q        ));
  run_lam 1    q   qh (REPR (fun q   -> evalo (app (abs varX (v varX)) q)        (v varY) ));
  run_lam 1    q   qh (REPR (fun q   -> evalo (app (abs varX q)        (v varY)) (v varY) ));
  run_lam 1    q   qh (REPR (fun q   -> evalo (app (v varX)            (v varX)) q        ));
  run_lam 1    q   qh (REPR (fun q   -> evalo (v varX) q                                  ));
  ()

let runL n = run_r GLam.reify GLam.show_llam n

let _ =
  runL 1   q   qh (REPR (fun q   -> evalo (app q (v varX)) (v varX)               ));
  runL 1  qr  qrh (REPR (fun q r -> evalo (app r q)        (v varX)               ));
  ()
