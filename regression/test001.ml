open GT
open OCanren
open OCanren.Std
open Tester
open Printf

let ilist xs = list (!!) xs
let just_a a = a === !!5

let occurs x =
  (x === List.cons !!1 x)

let two_vars x y = x===y
let a_and_b a =
  call_fresh (fun b ->
      (a === !!7) &&&
      conde [ (b === !!6); (b === !!5) ]
  )

let a_and_b' b =
  call_fresh (fun a ->
      (a === !!7) &&&
      conde [ (b === !!6); (b === !!5) ]
  )

let rec fives x =
  conde
    [ (x === !!5)
    ; defer (fives x)
    ]

let show_int       = show(int)
let show_int_list   = show(List.ground) (show int)
let show_intl_list  = show(List.logic ) (show(logic) (show int))

let rec appendo a b ab =
  conde
    [ ((a === nil ()) &&& (b === ab))
    ; fresh (h t ab')
        (a === h%t)
        (h%ab' === ab)
        (appendo t b ab')
    ]

let rec reverso a b =
  conde
    [ ((a === nil ()) &&& (b === nil ()))
    ; fresh (h t a')
        (a === h%t)
        (appendo a' !<h b)
        (defer (reverso t a'))
    ]

let run_exn eta = run_r (Std.List.prj_exn OCanren.prj_exn) eta
let _ =
  run_exn show_int_list  1  q qh (REPR (fun q   -> q === !!1 % q));
  run_exn show_int_list  1  q qh (REPR (fun q   -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4])   ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso q (ilist [1; 2; 3; 4])                  ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1; 2; 3; 4]) q                  ));
  run_exn show_int_list  2  q qh (REPR (fun q   -> reverso q (ilist [1])                           ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1]) q                           ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> occurs q                                        ))

let run_exn eta = run_r OCanren.prj_exn  eta
let _ =
  run_exn show_int       1  q qh (REPR (fun q   -> a_and_b q                                       ));
  run_exn show_int       2  q qh (REPR (fun q   -> a_and_b' q                                      ));
  run_exn show_int      10  q qh (REPR (fun q   -> fives q                                         ))

let runL eta = run_r (Std.List.reify OCanren.reify) show_intl_list eta
let runLL eta = run_r (Std.List.reify @@ Std.List.reify OCanren.reify) (GT.show Std.List.logic show_intl_list) eta

let _withFree =
  runL          1  q  qh (REPR (fun q   -> success                                      ));
  runL          1  q  qh (REPR (fun q   -> fresh (n) (q === n % Std.nil())              ));
  runLL         1  q  qh (REPR (fun q   -> fresh (a b c) (q === (a % b) % c)            ));
  runL          1  q  qh (REPR (fun q   -> reverso (ilist []) (ilist [])                ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          4 qr qrh (REPR (fun q r -> appendo q (ilist []) r                       ));
  runL          1  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          3  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL         10  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          1 qr qrh (REPR (fun q r -> two_vars q r                                 ));
  ()
