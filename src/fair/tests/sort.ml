open FairOCanren
open Printf
open GT

open List

module L = Stdlib.List

let (!!) = Logic.(!!)
let pair = Pair.pair

type 'a0 gnat =
| O 
| Succ of 'a0 

let o () = !! O
let succ x0 = !! (Succ (x0))

type nat = nat gnat

let show_nat nat =
  let rec helper = function
  | O -> 0
  | Succ x -> 1 + helper x in
  sprintf "%d" (helper nat)

let rec leo_rel a b q17 =
  ((a === (o ())) &&& (q17 === (!! true))) |||
    (call_fresh (fun a' -> (a === (succ a')) &&& (((b === (o ())) &&& (q17 === (!! false))) ||| (call_fresh (fun b' -> (b === (succ b')) &&& (leo a' b' q17))))))
and leo a b q17 = call Call.three "leo" leo_rel a b q17 

let rec minmax_rel a b q13 = call_fresh (fun q14 -> (leo a b q14) &&& (conde [(q14 === (!! true)) &&& (q13 === (pair a b)); (q14 === (!! false)) &&& (q13 === (pair b a))]))
and minmax a b q13 = call Call.three "minmax" minmax_rel a b q13 

let rec smallest_rel l q6 =
  (call_fresh (fun a -> (l === (a % (nil ()))) &&& (q6 === (pair a (nil ()))))) |||
    (call_fresh
       (fun x1 ->
          call_fresh
            (fun x2 ->
               call_fresh
                 (fun xs ->
                    call_fresh
                      (fun q9 ->
                         call_fresh
                           (fun min0 ->
                              call_fresh
                                (fun l' ->
                                   call_fresh
                                     (fun q11 ->
                                        call_fresh
                                          (fun min1 ->
                                             call_fresh
                                               (fun max1 ->
                                                  ?&
                                                    [l === (x1 % (x2 % xs));
                                                    q9 === (pair min0 l');
                                                    q11 === (pair min1 max1);
                                                    q6 === (pair min1 (max1 % l'));
                                                    smallest (x2 % xs) q9;
                                                    minmax x1 min0 q11]))))))))))
and smallest l q6 = call Call.two "smallest" smallest_rel l q6 
                          
let rec sort_rel l q0 =
  ((l === (nil ())) &&& (q0 === (nil ()))) |||
    (call_fresh
       (fun x ->
          call_fresh
            (fun xs ->
               call_fresh
                 (fun q3 ->
                    call_fresh (fun min -> call_fresh (fun l' -> call_fresh (fun q4 -> 
                      ?& [q3 === (pair min l'); 
                          q0 === (min % q4); 
                          
                          (* sort l' q4;
                          smallest l q3;  *)

                          
                          smallest l q3; 
                          sort l' q4;
                          
                          ])))))))
and sort l q0 = call Call.two "sort" sort_rel l q0

let sorto_info =  
  M.of_seq (L.to_seq ["leo", [true; true; false];
                      "minmax", [true; true; true];
                      "smallest", [true; false];
                      "sort", [false; true]])

let rec int2lnat n =
  if n <= 0 then o () else succ (int2lnat (n - 1))

let rec int_list2lnat_llist = function
| []      -> nil ()
| x :: xs -> int2lnat x % int_list2lnat_llist xs

open Benchmark

let n : int64 = 10L

let size = term_size
let strategy = fair_strategy sorto_info size
(* let strategy = lb_strategy *)

let queryF n a = sort (int_list2lnat_llist @@ L.rev @@ L.init n (fun x -> x)) a
let queryB n a = sort a (int_list2lnat_llist @@ L.init n (fun x -> x))

let rec fact n = if n <= 0 then 1 else n * fact (n - 1)

let testF n =
  let x = run 1 q (fun a -> a#reify (prj_exn Nat.prj_exn)) 
                    strategy (queryF n) in 
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground (show Nat.ground) a)) x

let testB n =
  let x = run (fact n) q (fun a -> a#reify (prj_exn Nat.prj_exn)) 
                    strategy (queryB n) in 
  printf "Amount: %d\n" (L.length x);
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground (show Nat.ground) a)) x

let run_test n q a = let _ = latency1 n q a in ()

(* let _ = *)
  (* testB 5 *)
  (* run_test n testF 30; *)
  (* run_test n testF 60; *)
  (* run_test n testF 90; *)
  (* run_test n testB 3; *)
  (* run_test n testB 4; *)
  (* run_test n testB 5; *)
  (* run_test n testB 6; *)
