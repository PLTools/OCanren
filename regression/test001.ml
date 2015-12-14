open GT
open MiniKanren
open Tester

let just_a a = a === !5

let a_and_b a =
  call_fresh (
    fun b ->
      (a === !7) &&&
      ((b === !6) ||| (b === !5))
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      (a === !7) &&&
      ((b === !6) ||| (b === !5))
  )

let rec fives x =
  (x === !5) |||
  (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =  
  ((a === !Nil) &&& (b === ab)) |||
  (call_fresh (fun h ->
    (call_fresh (fun t ->
      ((a === h%t) &&&
       (call_fresh (fun ab' ->
          (h%ab' === ab) &&& (appendo t b ab')
       ))
    )))
  ))
  
let rec reverso a b = 
  (conj (a === !Nil) (b === !Nil)) |||
  (call_fresh (fun h ->
    (call_fresh (fun t ->
        ((a === h%t) &&&
         (call_fresh (fun a' ->
            (appendo a' !<h b) &&& (reverso t a')
         ))
      )
  )
  )))

let show_int      = show(logic) (show int)
let show_int_list = show(logic) (show(llist) (show int))

let _ =
  run show_int_list empty_reifier  1  q (fun q   st -> REPR (appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4]) st), ["q", q]);  
  run show_int_list empty_reifier  4 qr (fun q r st -> REPR (appendo q (of_list []) r                          st), ["q", q; "r", r]);
  run show_int_list empty_reifier  1  q (fun q   st -> REPR (reverso q (of_list [1; 2; 3; 4])                  st), ["q", q]);
  run show_int_list empty_reifier  1  q (fun q   st -> REPR (reverso (of_list []) (of_list [])                 st), ["q", q]);
  run show_int_list empty_reifier  1  q (fun q   st -> REPR (reverso (of_list [1; 2; 3; 4]) q                  st), ["q", q]);
  run show_int_list empty_reifier  1  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]);
  run show_int_list empty_reifier  2  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]);
  run show_int_list empty_reifier  3  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]);
  run show_int_list empty_reifier 10  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]);
  run show_int_list empty_reifier  2  q (fun q   st -> REPR (reverso q (of_list [1])                           st), ["q", q]);
  run show_int_list empty_reifier  1  q (fun q   st -> REPR (reverso (of_list [1]) q                           st), ["q", q]);
  run show_int      empty_reifier  1  q (fun q   st -> REPR (a_and_b q                                         st), ["q", q]); 
  run show_int      empty_reifier  2  q (fun q   st -> REPR (a_and_b' q                                        st), ["q", q]); 
  run show_int      empty_reifier 10  q (fun q   st -> REPR (fives q                                           st), ["q", q])

