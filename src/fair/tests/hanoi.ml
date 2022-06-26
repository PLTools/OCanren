open FairOCanren
open Printf
open GT

open List

module L = Stdlib.List

let (!!) = Logic.(!!)
let pair = Pair.pair
let z () = Nat.o
let s x0 = Nat.s x0


type gstick =
  | One 
  | Two 
  | Thr 
let one () = !! One
let two () = !! Two
let thr () = !! Thr
type 'a gtriple =
  | Triple of 'a * 'a * 'a 
let triple x0 x1 x2 = !! (Triple (x0, x1, x2))

let rec notEqStick_rel x y q53 =
  conde
    [(x === (one ())) &&& (conde [(y === (one ())) &&& (q53 === (!! false)); (y === (two ())) &&& (q53 === (!! true)); (y === (thr ())) &&& (q53 === (!! true))]);
    (x === (two ())) &&& (conde [(y === (one ())) &&& (q53 === (!! true)); (y === (two ())) &&& (q53 === (!! false)); (y === (thr ())) &&& (q53 === (!! true))]);
    (x === (thr ())) &&& (conde [(y === (one ())) &&& (q53 === (!! true)); (y === (two ())) &&& (q53 === (!! true)); (y === (thr ())) &&& (q53 === (!! false))])]
and notEqStick x y q53 = call Call.three "notEqStick" notEqStick_rel x y q53

let rec isNil_rel l q48 = ((l === (nil ())) &&& (q48 === (!! true))) ||| (call_fresh (fun q50 -> call_fresh (fun q51 -> (l === (q50 % q51)) &&& (q48 === (!! false)))))
and isNil l q48 = call Call.two "isNil" isNil_rel l q48

let rec less_rel a b q44 =
  ((b === (z ())) &&& (q44 === (!! false))) |||
    (call_fresh (fun b' -> (b === (s b')) &&& (((a === (z ())) &&& (q44 === (!! true))) ||| (call_fresh (fun a' -> (a === (s a')) &&& (less a' b' q44))))))
and less a b q44 = call Call.three "less" less_rel a b q44

let rec get_rel name state q39 =
  call_fresh
    (fun s1 ->
       call_fresh
         (fun s2 ->
            call_fresh
              (fun s3 ->
                 (state === (triple s1 s2 s3)) &&& (conde [(name === (one ())) &&& (s1 === q39); (name === (two ())) &&& (s2 === q39); (name === (thr ())) &&& (s3 === q39)]))))
and get name state q39 = call Call.three "get" get_rel name state q39

let rec set_rel name stack state q34 =
  call_fresh
    (fun s1 ->
       call_fresh
         (fun s2 ->
            call_fresh
              (fun s3 ->
                 (state === (triple s1 s2 s3)) &&&
                   (conde
                      [(name === (one ())) &&& (q34 === (triple stack s2 s3));
                      (name === (two ())) &&& (q34 === (triple s1 stack s3));
                      (name === (thr ())) &&& (q34 === (triple s1 s2 stack))]))))
and set name stack state q34 = call Call.four "set" set_rel name stack state q34

let rec check_step_rel step state q22 =
  call_fresh
    (fun fromN ->
       call_fresh
         (fun toN ->
            call_fresh
              (fun q24 ->
                 ?&
                   [step === (pair fromN toN);
                   notEqStick fromN toN q24;
                   conde
                     [call_fresh
                        (fun q26 ->
                           ?&
                             [q24 === (!! true);
                             get fromN state q26;
                             ((q26 === (nil ())) &&& (q22 === (!! false))) |||
                               (call_fresh
                                  (fun x ->
                                     call_fresh
                                       (fun q28 ->
                                          call_fresh
                                            (fun q30 ->
                                               ?&
                                                 [q26 === (x % q28);
                                                 get toN state q30;
                                                 ((q30 === (nil ())) &&& (q22 === (!! true))) |||
                                                   (call_fresh (fun y -> call_fresh (fun q32 -> (q30 === (y % q32)) &&& (less x y q22))))]))))]);
                     (q24 === (!! false)) &&& (q22 === (!! false))]])))
and check_step step state q22 = call Call.three "check_step" check_step_rel step state q22

let rec one_step_rel step state q16 =
  call_fresh
    (fun fromN ->
       call_fresh
         (fun toN ->
            call_fresh
              (fun q18 ->
                 call_fresh
                   (fun x ->
                      call_fresh
                        (fun xs ->
                           call_fresh
                             (fun q19 ->
                                call_fresh
                                  (fun q20 ->
                                     ?& [step === (pair fromN toN); q18 === (x % xs); get fromN state q18; get toN state q19; set fromN xs state q20; set toN (x % q19) q20 q16])))))))
and one_step step state q16 = call Call.three "one_step" one_step_rel step state q16

let rec check_rel state steps q0 =
  (call_fresh
     (fun q1 ->
        call_fresh
          (fun q2 ->
             call_fresh
               (fun q7 ->
                  call_fresh
                    (fun q9 ->
                       ?&
                         [steps === (nil ());
                         get (one ()) state q7;
                         isNil q7 q1;
                         get (two ()) state q9;
                         isNil q9 q2;
                         conde [(q1 === (!! false)) &&& (q0 === (!! false)); (q1 === (!! true)) &&& (q0 === q2)]])))))
    |||
    (call_fresh
       (fun x ->
          call_fresh
            (fun xs ->
               call_fresh
                 (fun q12 ->
                    ?&
                      [steps === (x % xs);
                      check_step x state q12;
                      conde [call_fresh (fun q13 -> ?& [q12 === (!! true); one_step x state q13; check q13 xs q0]); (q12 === (!! false)) &&& (q0 === (!! false))]]))))
and check state steps q0 = call Call.three "check" check_rel state steps q0

(***********************************************************************************************)

let hanoi_info = M.of_seq (L.to_seq [
  "notEqStick", [false; false; false];
  "isNil", [false; false];
  "less", [true; true; false];
  "get", [false; false; false];
  "set", [false; false; false; false];
  "check_step", [false; false; false];
  "one_step", [false; false; false];
  "check", [false; true; false];
])

(***********************************************************************************************)

let show_stick = function
  | One -> "1"
  | Two -> "2"
  | Thr -> "3"

let show_step = function
  | (s, b) -> Printf.sprintf "%s-%s" (show_stick b) (show_stick s)

(***********************************************************************************************)

let rec llist f = function [] -> nil () | x :: xs -> f x % llist f xs

let rec toN n = if n = 0 then z () else s (toN (n - 1))

let gen_pin n =
  let rec gen_pin m =
    if m = n then nil () else toN m % gen_pin (m + 1) in
  gen_pin 0

let gen n = triple (gen_pin n) (nil ()) (nil ())

(***********************************************************************************************)

open Benchmark
let n : int64 = 10L

let size = term_size
(* let strategy = fair_strategy hanoi_info size *)
let strategy = lb_strategy

let answ = llist (fun (a, b) -> pair (a ()) (b ())) [(one, thr); (one, two); (thr, two); (one, thr); (two, one); (two, thr); (one, thr)]

let query a = check (gen 3) a !!true

let f _ =
  let x = run 1 q (fun a -> a#reify (prj_exn (Pair.prj_exn Logic.prj_exn Logic.prj_exn))) 
                    strategy query in 
  L.iter (fun a -> printf "\nlb, bottles 7 q true: \n%s\n" (show ground show_step a)) x


let _ = 
  (* f () *)
  latency1 n f () 
  (* show_steps (call_fresh query) strategy size fair_hprinter 25 *)
  (* show_steps (call_fresh query) strategy size (fun _ -> "") 40 *)