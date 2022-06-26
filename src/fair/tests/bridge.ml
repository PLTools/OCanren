open FairOCanren
open Printf
open GT

open List

module L = Stdlib.List

let (!!) = Logic.(!!)
let pair = Pair.pair
let none = Option.none
let some = Option.some


type 'a0 gpeano =
  | O 
  | S of 'a0 
let o () = !! O
let s x0 = !! (S (x0))
type gperson =
  | A 
  | B 
  | C 
  | D 
let a () = !! A
let b () = !! B
let c () = !! C
let d () = !! D

type 'a0 gstep =
  | One of 'a0 
  | Two of 'a0 * 'a0 
let one x0 = !! (One (x0))
let two x0 x1 = !! (Two (x0, x1))
type 'a0 gstate =
  | St of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 
let st x0 x1 x2 x3 x4 = !! (St (x0, x1, x2, x3, x4))


let rec eqBool_rel x y q129 =
  ((x === (!! true)) &&& (y === q129)) ||| ((x === (!! false)) &&& (conde [(y === (!! true)) &&& (q129 === (!! false)); (y === (!! false)) &&& (q129 === (!! true))]))
and eqBool x y q129 = call Call.three "eqBool" eqBool_rel x y q129 

let rec eqState_rel x y q103 =
  call_fresh
    (fun a1 ->
       call_fresh
         (fun a2 ->
            call_fresh
              (fun a3 ->
                 call_fresh
                   (fun a4 ->
                      call_fresh
                        (fun a5 ->
                           call_fresh
                             (fun b1 ->
                                call_fresh
                                  (fun b2 ->
                                     call_fresh
                                       (fun b3 ->
                                          call_fresh
                                            (fun b4 ->
                                               call_fresh
                                                 (fun b5 ->
                                                    call_fresh
                                                      (fun q105 ->
                                                         call_fresh
                                                           (fun q106 ->
                                                              call_fresh
                                                                (fun q111 ->
                                                                   call_fresh
                                                                    (fun q112 ->
                                                                    call_fresh
                                                                    (fun q117 ->
                                                                    call_fresh
                                                                    (fun q118 ->
                                                                    call_fresh
                                                                    (fun q123 ->
                                                                    call_fresh
                                                                    (fun q124 ->
                                                                    ?&
                                                                    [
                                                                    x === (st a1 a2 a3 a4 a5);
                                                                    y === (st b1 b2 b3 b4 b5);
                                                                    eqBool a1 b2 q105;
                                                                    eqBool a2 b2 q111;
                                                                    eqBool a3 b3 q117;
                                                                    eqBool a4 b4 q123;
                                                                    eqBool a5 b5 q124;
                                                                    conde [(q123 === (!! false)) &&& (q118 === (!! false)); (q123 === (!! true)) &&& (q118 === q124)];
                                                                    conde [(q117 === (!! false)) &&& (q112 === (!! false)); (q117 === (!! true)) &&& (q112 === q118)];
                                                                    conde [(q111 === (!! false)) &&& (q106 === (!! false)); (q111 === (!! true)) &&& (q106 === q112)];
                                                                    conde [(q105 === (!! false)) &&& (q103 === (!! false)); (q105 === (!! true)) &&& (q103 === q106)]]))))))))))))))))))
and eqState x y q103 = call Call.three "eqState" eqState_rel x y q103 

let rec greater_rel a0 b0 q99 =
  ((a0 === (o ())) &&& (q99 === (!! false))) |||
    (call_fresh (fun x -> (a0 === (s x)) &&& (((b0 === (o ())) &&& (q99 === (!! true))) ||| (call_fresh (fun y -> (b0 === (s y)) &&& (greater x y q99))))))
and greater a0 b0 q99 = call Call.three "greater" greater_rel a0 b0 q99

let rec grForPerson_rel x y q82 =
  conde
    [(x === (a ())) &&&
       (conde [(y === (a ())) &&& (q82 === (!! false)); (y === (b ())) &&& (q82 === (!! true)); (y === (c ())) &&& (q82 === (!! true)); (y === (d ())) &&& (q82 === (!! true))]);
    (x === (b ())) &&&
      (conde [(y === (a ())) &&& (q82 === (!! false)); (y === (b ())) &&& (q82 === (!! false)); (y === (c ())) &&& (q82 === (!! false)); (y === (d ())) &&& (q82 === (!! true))]);
    (x === (c ())) &&&
      (conde [(y === (a ())) &&& (q82 === (!! false)); (y === (b ())) &&& (q82 === (!! false)); (y === (c ())) &&& (q82 === (!! false)); (y === (d ())) &&& (q82 === (!! true))]);
    (x === (d ())) &&& (q82 === (!! false))]
and grForPerson x y q82 = call Call.three "grForPerson" grForPerson_rel x y q82

let rec max_rel a0 b0 q78 = 
  call_fresh (fun q79 -> (greater a0 b0 q79) &&& (conde [(q79 === (!! true)) &&& (a0 === q78); (q79 === (!! false)) &&& (b0 === q78)]))
and max a0 b0 q78 = call Call.three "max" max_rel a0 b0 q78

let rec add_rel a0 b0 q76 = 
  ((a0 === (o ())) &&& (b0 === q76)) ||| (call_fresh (fun x -> call_fresh (fun r -> (a0 === (s x)) &&& (q76 === (s r)) &&& (add x b0 r))))
and add a0 b0 q76 = call Call.three "add" add_rel a0 b0 q76

let rec checkPerson_rel state person q74 =
  call_fresh
    (fun l ->
       call_fresh
         (fun a0 ->
            call_fresh
              (fun b0 ->
                 call_fresh
                   (fun c0 ->
                      call_fresh
                        (fun d0 ->
                           (state === (st l a0 b0 c0 d0)) &&&
                             (conde
                                [(person === (a ())) &&& (eqBool a0 l q74);
                                (person === (b ())) &&& (eqBool b0 l q74);
                                (person === (c ())) &&& (eqBool c0 l q74);
                                (person === (d ())) &&& (eqBool d0 l q74)]))))))
and checkPerson state person q74 = call Call.three "checkPerson" checkPerson_rel state person q74

let rec checkStep_rel state step q61 =
  (call_fresh (fun p -> (step === (one p)) &&& (checkPerson state p q61))) |||
    (call_fresh
       (fun p ->
          call_fresh
            (fun q ->
               call_fresh
                 (fun q62 ->
                    call_fresh
                      (fun q63 ->
                         call_fresh
                           (fun q68 ->
                              call_fresh
                                (fun q69 ->
                                   ?&
                                     [step === (two p q);
                                     checkPerson state p q62;
                                     checkPerson state q q68;
                                     grForPerson p q q69;
                                     conde [(q68 === (!! false)) &&& (q63 === (!! false)); (q68 === (!! true)) &&& (q63 === q69)];
                                     conde [(q62 === (!! false)) &&& (q61 === (!! false)); (q62 === (!! true)) &&& (q61 === q63)]])))))))
and checkStep state step q61 = call Call.three "checkStep" checkStep_rel state step q61

let rec moveLight_rel state q56 =
  call_fresh
    (fun l ->
       call_fresh
         (fun a0 ->
            call_fresh
              (fun b0 ->
                 call_fresh
                   (fun c0 ->
                      call_fresh
                        (fun d0 ->
                           call_fresh
                             (fun q57 ->
                                ?&
                                  [state === (st l a0 b0 c0 d0);
                                  q56 === (st q57 a0 b0 c0 d0);
                                  conde [(l === (!! true)) &&& (q57 === (!! false)); (l === (!! false)) &&& (q57 === (!! true))]]))))))
and moveLight state q56 = call Call.two "moveLight" moveLight_rel state q56

let rec movePerson_rel state person q38 =
  call_fresh
    (fun l ->
       call_fresh
         (fun a0 ->
            call_fresh
              (fun b0 ->
                 call_fresh
                   (fun c0 ->
                      call_fresh
                        (fun d0 ->
                           (state === (st l a0 b0 c0 d0)) &&&
                             (conde
                                [call_fresh
                                   (fun q40 ->
                                      ?&
                                        [person === (a ());
                                        q38 === (st l q40 b0 c0 d0);
                                        conde [(a0 === (!! true)) &&& (q40 === (!! false)); (a0 === (!! false)) &&& (q40 === (!! true))]]);
                                call_fresh
                                  (fun q44 ->
                                     ?&
                                       [person === (b ());
                                       q38 === (st l a0 q44 c0 d0);
                                       conde [(b0 === (!! true)) &&& (q44 === (!! false)); (b0 === (!! false)) &&& (q44 === (!! true))]]);
                                call_fresh
                                  (fun q48 ->
                                     ?&
                                       [person === (c ());
                                       q38 === (st l a0 b0 q48 d0);
                                       conde [(c0 === (!! true)) &&& (q48 === (!! false)); (c0 === (!! false)) &&& (q48 === (!! true))]]);
                                call_fresh
                                  (fun q52 ->
                                     ?&
                                       [person === (d ());
                                       q38 === (st l a0 b0 c0 q52);
                                       conde [(d0 === (!! true)) &&& (q52 === (!! false)); (d0 === (!! false)) &&& (q52 === (!! true))]])]))))))
and movePerson state person q38 = call Call.three "movePerson" movePerson_rel state person q38

let rec step_rel state step q31 =
  (call_fresh (fun p -> call_fresh (fun q32 -> ?& [step === (one p); movePerson state p q32; moveLight q32 q31]))) |||
    (call_fresh
       (fun p -> call_fresh (fun q -> call_fresh (fun q34 -> call_fresh (fun q36 -> ?& [step === (two p q); movePerson state p q36; movePerson q36 q q34; moveLight q34 q31])))))
and step state step q31 = call Call.three "step" step_rel state step q31

let rec times_rel p q26 =
  conde
    [(p === (a ())) &&& (q26 === (s (o ())));
    (p === (b ())) &&& (q26 === (s (s (o ()))));
    (p === (c ())) &&& (q26 === (s (s (s (s (s (o ())))))));
    (p === (d ())) &&& (q26 === (s (s (s (s (s (s (s (s (s (s (o ()))))))))))))]
and times p q26 = call Call.two "times" times_rel p q26

let rec getTime_rel state q22 =
  (call_fresh (fun p -> (state === (one p)) &&& (times p q22))) |||
    (call_fresh (fun p -> call_fresh (fun q -> call_fresh (fun q23 -> call_fresh (fun q24 -> ?& [state === (two p q); times p q23; times q q24; max q23 q24 q22])))))
and getTime state q22 = call Call.two "getTime" getTime_rel state q22

let rec getAnswer_rec_rel answer state finish q4 =
  (call_fresh
     (fun x ->
        call_fresh
          (fun xs ->
             call_fresh
               (fun q6 ->
                  ?&
                    [answer === (x % xs);
                    checkStep state x q6;
                    conde
                      [call_fresh
                         (fun q8 ->
                            call_fresh
                              (fun q14 ->
                                 ?&
                                   [q6 === (!! true);
                                   step state x q14;
                                   getAnswer_rec xs q14 finish q8;         
                                   ((q8 === (none ())) &&& (q4 === (none ()))) |||
                                     (call_fresh
                                        (fun t1 ->
                                           call_fresh (fun q10 -> call_fresh (fun q12 -> ?& [q8 === (some t1); q4 === (some q10); getTime x q12; add q12 t1 q10]))));
                                           

                                  ]));
                      (q6 === (!! false)) &&& (q4 === (none ()))]]))))
    |||
    (call_fresh
       (fun q18 ->
          ?& [answer === (nil ()); eqState state finish q18; conde [(q18 === (!! true)) &&& (q4 === (some (o ()))); (q18 === (!! false)) &&& (q4 === (none ()))]]))
and getAnswer_rec answer state finish q4 = call Call.four "getAnswer_rec" getAnswer_rec_rel answer state finish q4

let rec getAnswer_rel answer q1 =
  call_fresh
    (fun start ->
       call_fresh
         (fun finish ->
            ?&
              [start === (st (!! true) (!! true) (!! true) (!! true) (!! true));
              finish === (st (!! false) (!! false) (!! false) (!! false) (!! false));
              (getAnswer_rec answer start finish q1)]))
and getAnswer answer q1 = call Call.two "getAnswer" getAnswer_rel answer q1

(*************************************************************)

let bridge_info = M.of_seq (L.to_seq [
  "eqBool", [false; false; false];
  "eqState", [false; false; false];
  "greater", [true; true; false];
  "grForPerson", [false; false; false];
  "max", [false; false; false];
  "add", [true; false; true];
  "checkPerson", [false; false; false];
  "checkStep", [false; false; false];
  "moveLight", [false; false];
  "movePerson", [false; false; false];
  "step", [false; false; false];
  "times", [false; false];
  "getTime", [false; false];
  "getAnswer_rec", [true; false; false; false];
  "getAnswer", [false; false] 
])

(*************************************************************)

let show_person = function
 | A -> "A"
 | B -> "B"
 | C -> "C"
 | D -> "D"

let show_step f = function
 | One x     -> f x
 | Two (x,y) -> Printf.sprintf "(%s, %s)" (f x) (f y)

let rec int2nat i = if i = 0 then o () else s @@ int2nat @@ i - 1

(*************************************************************)

open Benchmark
let n : int64 = 10L

let size = term_size
let strategy = fair_strategy bridge_info size
(* let strategy = lb_strategy *)

let query a = getAnswer a (some @@ int2nat 17)

let f _ =
  let x = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    strategy query in 
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground (show_step show_person) a)) x

(* let _ = *)
  (* f () *)
  (* latency1 n f ()  *)