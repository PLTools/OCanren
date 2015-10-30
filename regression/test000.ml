open GT
open MiniKanren

let run2 memo printer n goal =
  run (
    call_fresh (fun q -> 
      call_fresh (fun r st -> 
        let result = take ~n:n (goal q r st) in
        Printf.printf "%s {\n" memo;
        List.iter
          (fun st -> 
             Printf.printf "q=%s, r=%s\n" (printer st (refine st q)) (printer st (refine st r))
          )
          result;
        Printf.printf "}\n%!"
  )))

let run1 memo printer n goal =
  run (
    call_fresh (fun q st -> 
      let result = take ~n:n (goal q st) in
      Printf.printf "%s {\n" memo;
      List.iter
        (fun st ->        
           Printf.printf "q=%s\n" (printer st (refine st q))
        )
        result;
      Printf.printf "}\n%!"
  ))

let just_a a = a === 5

let a_and_b a =
  call_fresh (
    fun b ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

let rec fives x =
  disj (x === 5) 
       (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =
  disj
    (conj (a === []) (b === ab) )
    (call_fresh (fun h ->
      (call_fresh (fun t ->
        (conj (a === h::t)
           (call_fresh (fun ab' ->
              conj (h::ab' === ab)
                   (fun st -> Stream.from_fun (fun () -> appendo t b ab' st))
           ))
      )))
    ))
  
let rec reverso a b =
  disj
    (conj (a === []) (b === []))
    (call_fresh (fun h ->
      (call_fresh (fun t ->
          (conj (a === h::t)
              (call_fresh (fun a' ->
                 conj (fun st -> Stream.from_fun (fun () -> appendo a' [h] b st))
                      (fun st -> Stream.from_fun (fun () -> reverso t a' st))
              ))
        )
    )
    )))

let int_list st l = mkshow(list) (mkshow(int)) st l

let _ =
   run1 "appendo q [3; 4] [1; 2; 3; 4] max 1 result" int_list       1 (fun q   -> appendo q [3; 4] [1; 2; 3; 4]); 
   run2 "appendo q [] r max 4 results"               int_list       4 (fun q r -> appendo q [] r); 
   run1 "reverso q [1; 2; 3; 4] max 1 result"        int_list       1 (fun q   -> reverso q [1; 2; 3; 4]);  
   run1 "reverso [] [] max 1 result"                 int_list       1 (fun q   -> reverso [] []); 
   run1 "reverso [1; 2; 3; 4] q max 1 result"        int_list       1 (fun q   -> reverso [1; 2; 3; 4] q);  
   run1 "reverso q q max 1 result"                   int_list       1 (fun q   -> reverso q q); 
   run1 "reverso q q max 2 result"                   int_list       2 (fun q   -> reverso q q); 
   run1 "reverso q q max 3 result"                   int_list       3 (fun q   -> reverso q q); 
   run1 "reverso q q max 10 results"                 int_list      10 (fun q   -> reverso q q); 
   run1 "reverso q [1] max 2 results"                int_list       2 (fun q   -> reverso q [1]);
   run1 "reverso [1] q max 2 results"                int_list       1 (fun q   -> reverso [1] q); 
   run1 "just_a"                                     (mkshow(int))  1 (fun q   -> just_a q);
   run1 "a_and_b"                                    (mkshow(int))  1 (fun q   -> a_and_b q);
   run1 "a_and_b'"                                   (mkshow(int))  2 (fun q   -> a_and_b' q);
   run1 "fives"                                      (mkshow(int)) 10 (fun q   -> fives q)
