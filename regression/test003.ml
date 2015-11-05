open GT
open MiniKanren
open Tester

let just_a a = a === 5

let a_and_b a =
  fresh (b)
    (a === 7)
    (conde [b === 6; b === 5])

let a_and_b' b =
  fresh (a)
    (a === 7)
    (conde [b === 6; b === 5])
  
let rec fives x = (x === 5) ||| defer (fives x) 

let rec appendo a b ab =
  conde [
    (a === []) &&& (b === ab);
    fresh (h t ab')
      (a === h::t) 
      (h::ab' === ab) 
      (appendo t b ab')    
  ]
  
let rec reverso a b = 
  conde [
    (a === []) &&& (b === []);
    fresh (h t a')
      (a === h::t)
      (appendo a' [h] b)
      (reverso t a')
  ]

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
