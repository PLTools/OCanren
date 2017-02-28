open MiniKanren
open Tester
open Printf
open GT

let ilist xs = inj_list @@ List.map (!!) xs
let just_a a = a === !!5

let a_and_b a =
  call_fresh (fun b ->
      (a === !!7) &&&
      ((b === !!6) ||| (b === !!5))
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      (a === !!7) &&&
      ((b === !!6) ||| (b === !!5))
  )

let rec fives x =
  (x === !!5) |||
  (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =
  ((a === nil ()) &&& (b === ab)) |||
  Fresh.two (fun h t ->
      (a === h%t) &&&
      Fresh.one (fun ab' ->
          (h%ab' === ab) &&& (appendo t b ab')
      )
    )

let rec reverso a b =
  conde
    [ ((a === nil ()) &&& (b === nil ()))
    ; Fresh.two (fun h t ->
          (a === h%t) &&&
          (Fresh.one (fun a' ->
              (appendo a' !<h b) &&& (reverso t a')
          ))
      )
    ]

let show_int       = show(int)
let show_int_list  = (show(List.ground) (show int))
let show_intl_list = (show(List.logic ) (show(logic) (show int)))
let runL n         = runR (List.reify ManualReifiers.int_reifier) show_int_list show_intl_list n

let _ =
  run_exn show_int_list  1  q qh (REPR (fun q   -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4])   ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso q (ilist [1; 2; 3; 4])                  ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1; 2; 3; 4]) q                  ));
  run_exn show_int_list  2  q qh (REPR (fun q   -> reverso q (ilist [1])                           ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1]) q                           ));
  run_exn show_int       1  q qh (REPR (fun q   -> a_and_b q                                       ));
  run_exn show_int       2  q qh (REPR (fun q   -> a_and_b' q                                      ));
  run_exn show_int      10  q qh (REPR (fun q   -> fives q                                         ))

let _withFree =
  runL          1  q  qh (REPR (fun q   -> reverso (ilist []) (ilist [])                ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          4 qr qrh (REPR (fun q r -> appendo q (ilist []) r                       ));
  runL          1  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          3  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL         10  q  qh (REPR (fun q   -> reverso q q                                  ))
  
