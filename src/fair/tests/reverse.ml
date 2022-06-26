open FairOCanren
open Printf
open GT

open List

module L = Stdlib.List

let (!!) = Logic.(!!)
let pair = Pair.pair


let rec appendo_rel x y xy =
((x === nil ()) &&& (y === xy)) |||
call_fresh (fun e -> call_fresh (fun xs -> call_fresh (fun xys -> (?&)
  [(x === e % xs);
  (xy === e % xys);
  (appendo xs y xys)]))) 
and appendo x y xy = call Call.three "appendo" appendo_rel x y xy 


let rec reverso_rel xy yx =
 ((xy === nil ()) &&& (yx === nil ())) |||
 call_fresh (fun e -> call_fresh (fun xys -> call_fresh (fun yxs ->
   (xy === e % xys) &&&
   (reverso xys yxs) &&& 
   (appendo yxs (e % nil ()) yx) 
    
 )))
 and reverso xy yx = call Call.two "reverso" reverso_rel xy yx

let reverso_info = M.of_seq (L.to_seq ["appendo", [true; false; true];
                                       "reverso", [true; false]])

let rec llist = function
| []      -> nil ()
| x :: xs -> !!x % llist xs

(* let _ =
  let x = run 10 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    lb_strategy 
                    (fun a -> reverso (llist [1;2;3]) a) in 
  L.iter (fun a -> printf "lb, reverso [1;2;3] q: %s\n" (show ground (show int) a)) x *)

open Benchmark

let n : int64 = 10L

let size = term_size
let strategy = fair_strategy reverso_info size
(* let strategy = lb_strategy *)

let queryF n a = reverso (llist @@ L.init n (fun x -> x)) a
let queryB n a = reverso a (llist @@ L.init n (fun x -> x))


let testF n =
  let x = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    strategy (queryF n) in 
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground (show int) a)) x

let testB n =
  let x = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    strategy (queryB n) in 
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground (show int) a)) x

let run_test n q a = let _ = latency1 n q a in ()

(* let _ = *)
  (* run_test n testF 30; *)
  (* run_test n testF 60; *)
  (* run_test n testF 90; *)
  (* run_test n testB 30; *)
  (* run_test n testB 60; *)
  (* run_test n testB 90; *)