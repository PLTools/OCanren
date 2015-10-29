open GT
open MiniKanren

@type nat = O | S of nat with mkshow

let rec copy = function O -> O | S n -> S (copy n)

let run2 memo printer n goal =
  run (
    fresh (q r)
      (fun st -> 
        let result = take ~n:n (goal q r st) in
        Printf.printf "%s {\n" memo;
        List.iter
          (fun st -> 
             Printf.printf "q=%s, r=%s\n" (printer st (refine st q)) (printer st (refine st r))
          )
          result;
        Printf.printf "}\n%!"
  ))

let run1 memo printer n goal =
  run (
    fresh (q)
      (fun st ->
        let result = take ~n:n (goal q st) in
        Printf.printf "%s {\n" memo;
        List.iter
          (fun st ->        
             Printf.printf "q=%s\n" (printer st (refine st q))
          )
          result;
        Printf.printf "}\n%!"
  ))

let rec add x y =
  match x with O -> y | S n -> S (add n y)

let rec mul x y =
  match x with O -> O | S n -> add y (mul n y)

let rec addo x y z =
  conde [
    (x === O) &&& (z === y);
    fresh (x' z')
       (x === S x')
       (z === S z')
       (addo x' y z')
  ]

let rec mulo x y z =
  conde [
    (x === O) &&& (z === O);
    fresh (x' z') 
      (x === S x') 
      (addo y z' z) 
      (mulo x' y z')
  ]

let _ = 
   run1 "1 answer, addo O (S O) q"                  (mkshow nat)   1  (fun q   -> addo O (S O) q); 
   run1 "1 answer, addo (S O) (S O) q"              (mkshow nat)   1  (fun q   -> addo (S O) (S O) q); 
   run1 "2 answers, addo O (S O) q"                 (mkshow nat)   2  (fun q   -> addo O (S O) q); 
   run1 "2 answers, addo (S O) (S O) q"             (mkshow nat)   2  (fun q   -> addo (S O) (S O) q); 
   run1 "1 answer, addo q (S O) (S O)"              (mkshow nat)   1  (fun q   -> addo q (S O) (S O)); 
   run1 "1 answer, addo (S O) q (S O)"              (mkshow nat)   1  (fun q   -> addo (S O) q (S O)); 
   run1 "2 answers, addo q (S O) (S O)"             (mkshow nat)   2  (fun q   -> addo q (S O) (S O)); 
   run1 "2 answers, addo (S O) q (S O)"             (mkshow nat)   2  (fun q   -> addo (S O) q (S O)); 
   run2 "all answers, addo q r (S (S (S (S O))))"   (mkshow nat) (-1) (fun q r -> addo q r (S (S (S (S O))))); 
   
   run1 "1 answer, mulo O (S O) q"                  (mkshow nat)   1  (fun q   -> mulo O (S O) q);
   run1 "1 answer, mulo (S (S O)) (S (S O)) q"      (mkshow nat)   1  (fun q   -> mulo (S (S O)) (S (S O)) q);
   run1 "2 answers, mulo O (S O) q"                 (mkshow nat)   2  (fun q   -> mulo O (S O) q);

   run1 "1 answer, mulo q (S (S O)) (S (S O))"      (mkshow nat)   1  (fun q   -> mulo q (S (S O)) (S (S O)));
   run1 "1 answer, mulo q (S (S O)) (S (S (S O)))"  (mkshow nat)   1  (fun q   -> mulo q (S (S O)) (S (S (S O))));
   run1 "2 answers, mulo q (S (S O)) (S (S O))"     (mkshow nat)   2  (fun q   -> mulo q (S (S O)) (S (S O)));
   run1 "2 answers, mulo q (S (S O)) (S (S (S O)))" (mkshow nat)   2  (fun q   -> mulo q (S (S O)) (S (S (S O))));

   run1 "1 answer, mulo (S (S O)) q (S (S O))"      (mkshow nat)   1  (fun q   -> mulo (S (S O)) q (S (S O)));
   run1 "1 answer, mulo (S (S O)) q (S (S (S O)))"  (mkshow nat)   1  (fun q   -> mulo (S (S O)) q (S (S (S O))));
   run1 "2 answers, mulo (S (S O)) q (S (S O))"     (mkshow nat)   2  (fun q   -> mulo (S (S O)) q (S (S O)));
   run1 "2 answers, mulo (S (S O)) q (S (S (S O)))" (mkshow nat)   2  (fun q   -> mulo (S (S O)) q (S (S (S O))));

   run2 "1 answer, mulo q (S O) r"                  (mkshow nat)   1  (fun q r -> mulo q (S O) r);
   run2 "10 answers, mulo q (S O) r"                (mkshow nat)  10  (fun q r -> mulo q (S O) r);

   run2 "1 answer, mulo (S O) q r"                  (mkshow nat)   1  (fun q r -> mulo (S O) q r);
   run2 "10 answers, mulo (S O) q r"                (mkshow nat)  10  (fun q r -> mulo (S O) q r);

   run2 "1 answer, mulo q r O"    (mkshow nat)   1  (fun q r -> mulo q r O);
(*  
   run2 "1 answer, mulo q r (S O)"    (mkshow nat)   1  (fun q r -> mulo q r (S O)); 
  *)
 run1 "1 answer, mulo (S O) (S O) q"    (mkshow nat)   1  (fun q -> mulo (S O) (S O) q); 

(*
   run2 "1 answer, mulo q r (S (S (S (S O))))"    (mkshow nat)   1  (fun q r -> mulo q r (S (S (S (S O)))));
*)
   let divisers n = 
     run (
       fresh (q r)
         (fun st -> List.length (take ~n:(-1) (mulo q r n st)))
     )
   in

(*   Printf.printf "divisers O=%d\n" (divisers O); *)
  ()
