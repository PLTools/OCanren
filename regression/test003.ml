open GT
open MiniKanren
open Tester

let just_a a = a === !5

let a_and_b a =
  fresh (b)
    (a === !7)
    (conde [b === !6; b === !5])

let a_and_b' b =
  fresh (a)
    (a === !7)
    (conde [b === !6; b === !5])
  
let rec fives x = (x === !5) ||| defer (fives x) 

let rec appendo a b ab =
  conde [
    (a === !Nil) &&& (b === ab);
    fresh (h t ab')
      (a === h%t) 
      (h%ab' === ab) 
      (appendo t b ab')    
  ]
  
let rec reverso a b = 
  conde [
    (a === !Nil) &&& (b === !Nil);
    fresh (h t a')
      (a === h%t)
      (appendo a' !<h b)
      (reverso t a')
  ]

let show_int      = show(logic) (show int)
let show_int_list = show(logic) (show(llist) (show int))

let _ =
  run show_int_list  1  q (REPR (fun q   -> appendo q (inj_list [3; 4]) (inj_list [1; 2; 3; 4]))) qh;
  run show_int_list  4 qr (REPR (fun q r -> appendo q (inj_list []) r                         )) qrh;
  run show_int_list  1  q (REPR (fun q   -> reverso q (inj_list [1; 2; 3; 4])                 )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list []) (inj_list [])               )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1; 2; 3; 4]) q                 )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso q q                                       )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q q                                       )) qh;
  run show_int_list  3  q (REPR (fun q   -> reverso q q                                       )) qh;
  run show_int_list 10  q (REPR (fun q   -> reverso q q                                       )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q (inj_list [1])                          )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1]) q                          )) qh;
  run show_int       1  q (REPR (fun q   -> a_and_b q                                         )) qh;
  run show_int       2  q (REPR (fun q   -> a_and_b' q                                        )) qh;
  run show_int      10  q (REPR (fun q   -> fives q                                           )) qh
