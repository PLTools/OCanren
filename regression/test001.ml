(* Testing the most simple relations here: appendo, reverso, etc. *)
open MiniKanren
open Tester
open Printf

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
(* 
module Glist = struct
  module X = struct
    type ('a, 'b) t = ('a, 'b) llist
    let fmap f g = function
    | Nil -> Nil
    | Cons (x,xs) -> Cons (f x, g xs)
  end

  include X
  include Fmap2(X)
end *)

let show_int = GT.(show int)
let show_int_list  = GT.(show List.ground (show int))
let show_intl_list = GT.(show List.logic (show_logic (show int)))
let runL n = runR (List.reifier ManualReifiers.int_reifier) show_int_list show_intl_list n

let _ =
  run_exn show_int_list  1  q qh (REPR (fun q   -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4])   ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso q (ilist [1; 2; 3; 4])                  ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1; 2; 3; 4]) q                  ));
  run_exn show_int_list  2  q qh (REPR (fun q   -> reverso q (ilist [1])                           ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1]) q                           ));
  run_exn show_int       1  q qh (REPR (fun q   -> a_and_b q                                          ));
  run_exn show_int       2  q qh (REPR (fun q   -> a_and_b' q                                         ));
  run_exn show_int      10  q qh (REPR (fun q   -> fives q                                            ));
  ()

let _withFree =
  runL          1  q  qh (REPR (fun q   -> reverso (ilist []) (ilist [])                ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          4 qr qrh (REPR (fun q r -> appendo q (ilist []) r                       ));
  runL          1  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          3  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL         10  q  qh (REPR (fun q   -> reverso q q                                  ));
  ()
