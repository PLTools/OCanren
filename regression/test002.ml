open GT
open MiniKanren
open Tester

let just_a a = a === !5

let a_and_b a =
  call_fresh (
    fun b ->
      (a === !7) &&&
      conde [b === !6; b === !5]
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      (a === !7) &&&
      conde [b === !6; b === !5]
  )

let rec fives x =
  (x === !5) |||
  (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =
  ?| [  
  (a === !Nil) &&& (b === ab);
  call_fresh (fun h ->
    (call_fresh (fun t ->
      ((a === h%t) &&&
       (call_fresh (fun ab' ->
          (h%ab' === ab) &&& (appendo t b ab')
       ))
    ))))
  ]
  
let rec reverso a b = 
  conde [
    (a === !Nil) &&& (b === !Nil);
    call_fresh (fun h ->
      (call_fresh (fun t ->
          ((a === h%t) &&&
           (call_fresh (fun a' ->
              ?& [appendo a' !<h b; reverso t a']
           ))
        )  
    )))]

let show_int      = show(logic) (show int)
let show_int_list = show(List.logic) show_int

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
