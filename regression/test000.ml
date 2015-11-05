open GT
open MiniKanren
open Tester

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
  run int_list       1 q  (fun q   st -> REPR (appendo q [3; 4] [1; 2; 3; 4] st), ["q", q]);
  run int_list       4 qp (fun q p st -> REPR (appendo q [] p st)               , ["q", q; "p", p]);
  run int_list       1 q  (fun q   st -> REPR (reverso q [1; 2; 3; 4] st)       , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso [] [] st)                , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso [1; 2; 3; 4] q st)       , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list       2 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list       3 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list      10 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list       2 q  (fun q   st -> REPR (reverso q [1] st)                , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso [1] q st)                , ["q", q]);
  run (mkshow(int))  1 q  (fun q   st -> REPR (a_and_b q st)                    , ["q", q]); 
  run (mkshow(int))  2 q  (fun q   st -> REPR (a_and_b' q st)                   , ["q", q]); 
  run (mkshow(int)) 10 q  (fun q   st -> REPR (fives q st)                      , ["q", q])

