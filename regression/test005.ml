open GT
open MiniKanren
open Tester

@type lam = X of string | App of lam * lam | Abs of string * lam with show, mkshow
@type typ = V of string | Arr of typ * typ with show, mkshow

let rec lookupo a g t =
  fresh (h a' t' tl) 
    (g === h::tl)
    ((a', t') === h)
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])      

let mkshow_env = mkshow list (mkshow pair (mkshow string) (mkshow typ))

let _ =
  run (mkshow typ)    1 q (fun q st -> REPR (lookupo "x" [] q st), ["q", q]);
  run (mkshow typ)    1 q (fun q st -> REPR (lookupo "x" ["x", V "x"] q st), ["q", q]);
  run (mkshow typ)    1 q (fun q st -> REPR (lookupo "x" ["y", V "y"; "x", V "x"] q st), ["q", q]);
  run (mkshow string) 1 q (fun q st -> REPR (lookupo q ["y", V "y"; "x", V "x"] (V "x") st), ["q", q]);
  run (mkshow string) 1 q (fun q st -> REPR (lookupo q ["y", V "y"; "x", V "x"] (V "y") st), ["q", q]);
  run  mkshow_env     1 q (fun q st -> REPR (lookupo "x" q (V "y") st), ["q", q]);
  run  mkshow_env     5 q (fun q st -> REPR (lookupo "x" q (V "y") st), ["q", q]);
   

(*
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

   run2 "1 answer, mulo q r O"                      (mkshow nat)   1  (fun q r -> mulo q r O);
   run2 "1 answer, mulo q r (S O)"                  (mkshow nat)   1  (fun q r -> mulo q r (S O)); 
   run1 "1 answer, mulo (S O) (S O) q"              (mkshow nat)   1  (fun q   -> mulo (S O) (S O) q); 

   run2 "1 answer, mulo q r (S (S (S (S O))))"      (mkshow nat)   1  (fun q r -> mulo q r (S (S (S (S O)))));
   run2 "3 answers, mulo q r (S (S (S (S O))))"     (mkshow nat)   3  (fun q r -> mulo q r (S (S (S (S O)))));
    
   run3 "1 answer, mulo q r t"                      (mkshow nat)   1  (fun q r t -> mulo q r t);
   run3 "10 answers, mulo q r t"                    (mkshow nat)   10 (fun q r t -> mulo q r t)
*)
  ()
