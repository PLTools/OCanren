open MiniKanren
open Tester
open Printf

let inj_int n : (int,int) fancy = inj@@lift n
let ilist xs = inj_list @@ List.map inj_int xs
let just_a a = a === inj@@lift 5

let a_and_b a =
  call_fresh (fun b ->
      (a === inj@@lift 7) &&&
      ((b === inj@@lift 6) ||| (b === inj@@lift 5))
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      (a === inj@@lift 7) &&&
      ((b === inj@@lift 6) ||| (b === inj@@lift 5))
  )

let rec fives x =
  (x === inj@@lift 5) |||
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

let show_int = string_of_int
let show_int_list : int list -> string = GT.show(GT.list) show_int



let _ =
  run_exn show_int_list  1  q (REPR (fun q   -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4]))) qh;
  run_exn show_int_list  1  q (REPR (fun q   -> reverso q (ilist [1; 2; 3; 4])                  )) qh;
  run_exn show_int_list  1  q (REPR (fun q   -> reverso (ilist [1; 2; 3; 4]) q                  )) qh;
  run_exn show_int_list  2  q (REPR (fun q   -> reverso q (ilist [1])                           )) qh;
  run_exn show_int_list  1  q (REPR (fun q   -> reverso (ilist [1]) q                           )) qh;
  run_exn show_int       1  q (REPR (fun q   -> a_and_b q                                          )) qh;
  run_exn show_int       2  q (REPR (fun q   -> a_and_b' q                                         )) qh;
  run_exn show_int      10  q (REPR (fun q   -> fives q                                            )) qh;
  ()

let _withFree () =
  run_exn show_int_list  1  q (REPR (fun q   -> reverso (ilist []) (ilist [])                )) qh;
  run_exn show_int_list  4 qr (REPR (fun q r -> appendo q (ilist []) r                          )) qrh;
  run_exn show_int_list  1  q (REPR (fun q   -> reverso q q                                        )) qh;
  run_exn show_int_list  2  q (REPR (fun q   -> reverso q q                                        )) qh;
  run_exn show_int_list  3  q (REPR (fun q   -> reverso q q                                        )) qh;
  run_exn show_int_list 10  q (REPR (fun q   -> reverso q q                                        )) qh;
  ()
