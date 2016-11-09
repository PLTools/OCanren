1open GT
open MiniKanren
open Tester

let just_a a = a === (5 |> lift |> inj)
(*
let a_and_b a =
  call_fresh (
    fun b ->
      conj (a === !7)
           (disj (b === !6)
                 (b === !5)
           )
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      conj (a === !7)
           (disj (b === !6)
                 (b === !5)
           )
  )

let rec fives x =
  disj (x === !5)       
       (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =
  disj
    (conj (a === !Nil) (b === ab) )
    (call_fresh (fun h ->
      (call_fresh (fun t ->
        (conj (a === h % t)
           (call_fresh (fun ab' ->
              conj (h % ab' === ab)
                   (appendo t b ab')
           ))
      )))
    ))

let rec reverso a b =
  disj
    (conj (a === !Nil) (b === !Nil))
    (call_fresh (fun h ->
      (call_fresh (fun t ->
          (conj (a === h % t)
                (call_fresh (fun a' ->
                   conj (appendo a' !< h b)
                        (reverso t a')
                ))
        )
    )
    )))
*)
(*
let show_int      = show(logic) (show int)
let show_int_list = show(List.logic) show_int
*)
;;

let rec show_list l = show(llist) (show(int)) show_list l;;

@type 'a test = A of 'a with show

module LTest = Fmap (struct type 'a t = 'a test let fmap f = function A x -> A (f x)  end)

let rec show_test t = show(test) (show(int)) t

let _ =
  MiniKanren.run q 
    (fun q -> inj (LTest.fmap (A q)) === inj (LTest.fmap (A (inj (lift 5)))))
    (fun qs -> Printf.printf "%s\n" (show(int) @@ Stream.hd qs))
(*
  run show_int_list  1  q (REPR (fun q   -> appendo q (inj_list [3; 4]) (inj_list [1; 2; 3; 4]))) qh;
  run show_int_list  4 qr (REPR (fun q r -> appendo q (inj_list []) r                          )) qrh;
  run show_int_list  1  q (REPR (fun q   -> reverso q (inj_list [1; 2; 3; 4])                  )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list []) (inj_list [])                )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1; 2; 3; 4]) q                  )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  3  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list 10  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q (inj_list [1])                           )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1]) q                           )) qh;
  run show_int       1  q (REPR (fun q   -> a_and_b q                                          )) qh;
  run show_int       2  q (REPR (fun q   -> a_and_b' q                                         )) qh;
  run show_int      10  q (REPR (fun q   -> fives q                                            )) qh

*)
