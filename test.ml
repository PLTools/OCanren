open GT
open MiniKanren
open Printf

@type t = A of int | B of string | C of t * t with show, minikanren

let run memo printer n goal =
  let q, e   = Env.fresh (Env.empty ())  in
  let r, e   = Env.fresh e               in
  let st     = e, Subst.empty            in
  let result = Stream.take n (goal q r st) in
  Printf.printf "%s {\n" memo;
  List.iter
    (fun (env, subst) ->
        Printf.printf "%s, %s\n" (printer env (Subst.walk' env q subst)) (printer env (Subst.walk' env r subst))
    )
    result;
  Printf.printf "}\n%!"

let run1 memo printer = run memo printer 1
let run2 memo printer = run memo printer 2

let just_a a = a === 5

let a_and_b  a =
  fresh (
    fun b ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

let a_and_b' b =
  fresh (
    fun a ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

let rec fives x =
  disj (x === 5)
       (fun st -> Stream.from_fun (fun () -> fives x st))

let int_list e = show_list e show_int

let rec appendo a b ab ((env, subst) as st) =
  logn "show [] = '%s'" (generic_show !![]);
  logn "show 5  = '%s'" (generic_show !!5);
  logn "show [5] = '%s'" (generic_show !![5]);
  logn "appendo%!";
  logn " %s, %s, %s" (generic_show !!a) (generic_show !!b) (generic_show !!ab);


  disj
    (conj (a === (logn "1st uni\n%!"; []) ) (logn "2st uni\n%!"; (b === ab)) )
    (fresh (fun h ->
      (fresh (fun t ->
        (conj (a === h::t)
           (fresh (fun ab' ->
              conj (h::ab' === ab)
                   (appendo t b ab')
           ))
      )))
    ))
  st

let rec reverso a b ((env, subst) as st) =
  logn "reverso: %s %s\n" (generic_show !!a) (generic_show !!b); flush stdout;
(*
  logn "reverso: %s, " (generic_show !!b); flush stdout;
  logn "%s\n" (int_list env b); flush stdout;
*)
  fresh (fun h ->
    fresh (fun t ->
      disj
        (conj (a === []) (b === []))
        (conj (a === h::t)
              (fresh (fun a' ->
                 conj (appendo a' [h] b)
                      (reverso t a')
              ))
        )
    )
  ) st

let _ =
  (* run "appendo" int_list 1 (fun q st -> appendo q [3; 4] [1; 2; 3; 4] st); *)
  run1 "appendo" int_list (fun q r st -> appendo q [] r st);
  run2 "appendo" int_list (fun q r st -> appendo q [] r st);
(*  run "reverso"  int_list 1  (fun q st -> reverso [1] q st)*)(*;
  run "reverso"  int_list 1  (fun q st -> reverso [1; 2; 3; 4] q st);
  run "just_a"   show_int 1  (fun q st -> just_a q st);
  run "a_and_b"  show_int 1  (fun q st -> a_and_b q st);
  run "a_and_b'" show_int 2  (fun q st -> a_and_b' q st);
  run "fives"    show_int 10 (fun q st -> fives q st)*)
  ()
