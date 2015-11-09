open GT
open MiniKanren
open Tester

@type ('a, 'b) list = Nil | Cons of 'a * 'b with mkshow

let (-:-) a b = Cons (a, b)

let just_a a = a === ~?5

let a_and_b a =
  call_fresh (
    fun b ->
      conj (a === ~?7)
           (disj (b === ~?6)
                 (b === ~?5)
           )
  )

let a_and_b' b =
  call_fresh (
    fun a ->
      conj (a === ~?7)
           (disj (b === ~?6)
                 (b === ~?5)
           )
  )

let rec fives x =
  disj (x === ~?5) 
       (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =
  disj
    (conj (a === ~?Nil) (b === ab) )
    (call_fresh (fun h ->
      (call_fresh (fun t ->
        (conj (a === ~?(Cons (h, t)))
           (call_fresh (fun ab' ->
              conj (~?(Cons (h, ab')) === ab)
                   (appendo t b ab')
           ))
      )))
    ))

let rec reverso a b =
  disj
    (conj (a === ~?Nil) (b === ~?Nil))
    (call_fresh (fun h ->
      (call_fresh (fun t ->
          (conj (a === ~?(Cons (h, t)))
              (call_fresh (fun a' ->
                 conj (appendo a' ~?(Cons (h, ~?Nil)) b)
                      (reverso t a')
              ))
        )
    )
    )))

let rec int_list st l = mkshow(list) (mkshow(int)) int_list st l

let skip st l = "skipped"

let rec of_list = function
  | [] -> ~? Nil
  | x::xs -> ~?(Cons (~?x, of_list xs))

let _ =
  run (*int_list*) skip       1 q  (fun q   st -> REPR (appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4]) st), ["q", q]);
(*
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
*)
  run (mkshow(int))  1 q  (fun q   st -> REPR (a_and_b q st)                    , ["q", q]); 
  run (mkshow(int))  2 q  (fun q   st -> REPR (a_and_b' q st)                   , ["q", q]); 
  run (mkshow(int)) 10 q  (fun q   st -> REPR (fives q st)                      , ["q", q])

