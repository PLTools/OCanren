open FairOCanren
open Printf
open GT

open List

module L = Stdlib.List

let (!!) = Logic.(!!)
let pair = Pair.pair


type gbottle =
  | Fst 
  | Snd 
let fst_ () = !! Fst
let snd_ () = !! Snd

type gstepType =
  | Fill 
  | Empty 
  | Pour 
let fill () = !! Fill
let empty () = !! Empty
let pour () = !! Pour

type 'a0 gnat =
  | O 
  | S of 'a0 
let o () = !! O
let s x0 = !! (S (x0))

let show_bottle = function
 | Fst -> "1"
 | Snd -> "2"

let show_stepType = function
 | Fill  -> "F"
 | Empty -> "E"
 | Pour  -> "P"

let show_step = function
 | (s, b) -> Printf.sprintf "%s%s" (show_bottle b) (show_stepType s)

let rec eqNat_rel a b q78 =
  ((a === (o ())) &&& (((b === (o ())) &&& (q78 === (!! true))) ||| (call_fresh (fun q81 -> (b === (s q81)) &&& (q78 === (!! false)))))) |||
    (call_fresh (fun x -> (a === (s x)) &&& (((b === (o ())) &&& (q78 === (!! false))) ||| (call_fresh (fun y -> (b === (s y)) &&& (eqNat x y q78))))))
and eqNat a b q78 = call Call.three "eqNat" eqNat_rel a b q78 


let rec add_rel a b q76 = ((a === (o ())) &&& (b === q76)) ||| 
   (call_fresh (fun x -> call_fresh (fun q78 -> ?& [a === (s x); q76 === (s q78); add x b q78])))
and add a b q76 = call Call.three "add" add_rel a b q76 

let rec greater_rel a b q72 =
  ((a === (o ())) &&& (q72 === (!! false))) |||
    (call_fresh (fun x -> (a === (s x)) &&& (((b === (o ())) &&& (q72 === (!! true))) ||| (call_fresh (fun y -> (b === (s y)) &&& (greater x y q72))))))
and greater a b q72 = call Call.three "greater" greater_rel a b q72 

let rec sub_rel a b q68 =
  ((b === (o ())) &&& (a === q68)) |||
    (call_fresh (fun y -> (b === (s y)) &&& (((a === (o ())) &&& (q68 === (o ()))) ||| (call_fresh (fun x -> (a === (s x)) &&& (sub x y q68))))))
and sub a b q68 = call Call.three "sub" sub_rel a b q68 

let rec anotherBottle_rel b q65 = ((b === (fst_ ())) &&& (q65 === (snd_ ()))) ||| ((b === (snd_ ())) &&& (q65 === (fst_ ())))
and anotherBottle b q65  = call Call.two "anotherBottle" anotherBottle_rel b q65 


let rec createState_rel bottle lvl1 lvl2 q62 = ((bottle === (fst_ ())) &&& (q62 === (pair lvl1 lvl2))) ||| ((bottle === (snd_ ())) &&& (q62 === (pair lvl2 lvl1)))
and createState bottle lvl1 lvl2 q62 = call Call.four "createState" createState_rel bottle lvl1 lvl2 q62 

let rec capacities_rel b q59 = ((b === (fst_ ())) &&& (q59 === (s (s (s (s (o ()))))))) ||| ((b === (snd_ ())) &&& (q59 === (s (s (s (s (s (s (s (s (s (o ()))))))))))))
and capacities b q59 = call Call.two "capacities" capacities_rel b q59

let rec checkStep_rel state0 step0 q33 =
  call_fresh
    (fun f ->
       call_fresh
         (fun s ->
            call_fresh
              (fun t ->
                 call_fresh
                   (fun b ->
                      call_fresh
                        (fun lvl1 ->
                           call_fresh
                             (fun lvl2 ->
                                ?&
                                  [state0 === (pair f s);
                                  step0 === (pair t b);
                                  ((b === (fst_ ())) &&& (f === lvl1)) ||| ((b === (snd_ ())) &&& (s === lvl1));
                                  ((b === (fst_ ())) &&& (s === lvl2)) ||| ((b === (snd_ ())) &&& (f === lvl2));
                                  conde
                                    [(t === (fill ())) &&& (eqNat lvl1 (o ()) q33);
                                    call_fresh (fun q44 -> ?& [t === (empty ()); capacities b q44; eqNat lvl1 q44 q33]);
                                    call_fresh
                                      (fun b' ->
                                         call_fresh
                                           (fun q47 ->
                                              call_fresh
                                                (fun q51 ->
                                                   call_fresh
                                                     (fun q52 ->
                                                        call_fresh
                                                          (fun q57 ->
                                                             ?&
                                                               [t === (pour ());
                                                               anotherBottle b b';
                                                               eqNat lvl1 (o ()) q51;
                                                               capacities b' q57;
                                                               eqNat lvl2 q57 q52;
                                                               conde [(q51 === (!! true)) &&& (q47 === (!! true)); (q51 === (!! false)) &&& (q47 === q52)];
                                                               conde [(q47 === (!! true)) &&& (q33 === (!! false)); (q47 === (!! false)) &&& (q33 === (!! true))]])))))]]))))))
and checkStep state0 step0 q33 = call Call.three "checkStep" checkStep_rel state0 step0 q33

let rec doStep_rel state0 step0 q16 =
  call_fresh
    (fun f ->
       call_fresh
         (fun s ->
            call_fresh
              (fun t ->
                 call_fresh
                   (fun b ->
                      call_fresh
                        (fun lvl2 ->
                           ?&
                             [state0 === (pair f s);
                             step0 === (pair t b);
                             ((b === (fst_ ())) &&& (s === lvl2)) ||| ((b === (snd_ ())) &&& (f === lvl2));
                             conde
                               [call_fresh (fun q23 -> ?& [t === (fill ()); capacities b q23; createState b q23 lvl2 q16]);
                               (t === (empty ())) &&& (createState b (o ()) lvl2 q16);
                               call_fresh
                                 (fun sum ->
                                    call_fresh
                                      (fun cap2 ->
                                         call_fresh
                                           (fun q26 ->
                                              call_fresh
                                                (fun q30 ->
                                                   ?&
                                                     [t === (pour ());
                                                     add f s sum;
                                                     anotherBottle b q26;
                                                     capacities q26 cap2;
                                                     greater sum cap2 q30;
                                                     conde
                                                       [call_fresh (fun q31 -> ?& [q30 === (!! true); sub sum cap2 q31; createState b q31 cap2 q16]);
                                                       (q30 === (!! false)) &&& (createState b (o ()) sum q16)]]))))]])))))
and doStep state0 step0 q16 = call Call.three "doStep" doStep_rel state0 step0 q16

let rec isFinishState_rel state0 reqLvl q9 =
  call_fresh
    (fun f ->
       call_fresh
         (fun s ->
            call_fresh
              (fun q10 ->
                 call_fresh
                   (fun q11 ->
                      ?& [state0 === (pair f s); eqNat f reqLvl q10; eqNat s reqLvl q11; conde [(q10 === (!! true)) &&& (q9 === (!! true)); (q10 === (!! false)) &&& (q9 === q11)]]))))
and isFinishState state0 reqLvl q9 = call Call.three "isFinishState" isFinishState_rel state0 reqLvl q9

let rec checkAnswer_rec_rel state0 answer reqLvl q0 =
  ((answer === (nil ())) &&& (isFinishState state0 reqLvl q0)) |||
    (call_fresh
       (fun x ->
          call_fresh
            (fun xs ->
               call_fresh
                 (fun q2 ->
                    ?&
                      [answer === (x % xs);
                      checkStep state0 x q2;
                      conde [call_fresh (fun q3 -> ?& [q2 === (!! true); checkAnswer_rec q3 xs reqLvl q0; doStep state0 x q3]); (q2 === (!! false)) &&& (q0 === (!! false))]]))))
and checkAnswer_rec state0 answer reqLvl q0 = call Call.four "checkAnswer_rec" checkAnswer_rec_rel state0 answer reqLvl q0

let rec checkAnswer_rel answer reqLvl q8 =
  call_fresh (fun startState -> (startState === (pair (o ()) (o ()))) &&& (checkAnswer_rec startState answer reqLvl q8))
and checkAnswer answer reqLvl q8 = call Call.three "checkAnswer" checkAnswer_rel answer reqLvl q8

(***********************************************************************************************)

let bottle_info = M.of_seq (L.to_seq [
  "eqNat", [true; true; false];
  "add", [true; false; true];
  "greater", [true; true; false];
  "sub", [true; true; false];
  "anotherBottle", [false; false];
  "createState", [false; false; false; false];
  "capacities", [false; false];
  "checkStep", [false; false; false];
  "doStep", [false; false; false];
  "isFinishState", [false; false; false];
  "checkAnswer_rec", [false; true; false; false];
  "checkAnswer", [false; false; false] 
])

(***********************************************************************************************)

let rec int2lnat n =
  if n <= 0 then o () else s (int2lnat (n - 1))

(***********************************************************************************************)

open Benchmark
let n : int64 = 10L

let size = term_size
(* let strategy = fair_strategy bottle_info size *)
let strategy = lb_strategy

let query a = checkAnswer a (int2lnat 7) !!true

let f _ =
  let x = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    strategy query in 
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground show_step a)) x


(* let _ =  *)
  (* f () *)
  (* latency1 n f ()  *)
  (* show_steps (call_fresh query) strategy size fair_hprinter 25 *)
  (* show_steps (call_fresh query) strategy size (fun _ -> "") 40 *)