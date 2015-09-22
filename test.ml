open GT
open MiniKanren 

@type t = A of int | B of string | C of t * t with show, minikanren

let run memo printer n goal =
  let q, e   = Env.fresh (Env.empty ()) in
  let st     = e, Subst.empty in 
  let result = Stream.take n (goal q st) in
  Printf.printf "%s {\n" memo;
  List.iter 
    (fun (env, subst) ->
        Printf.printf "%s\n" (printer env (Subst.walk' env q subst))
    )
    result;
  Printf.printf "}\n";
  flush stdout

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
       
let rec appendo a b ab =
  fresh (fun h ->
    fresh (fun t ->
      disj 
        (conj (a === []) (ab === b))
        (conj (a === h::t)
           (fresh (fun ab' ->
              conj (h::ab' === ab)
                   (appendo t b ab')
           ))
        )
    )
  )

let rec reverso a b =
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
  )

let int_list e = show_list e show_int

let _ = 
  run "appendo"  int_list 1  (fun q st -> appendo q [3; 4] [1; 2; 3; 4] st);
  run "reverso"  int_list 1  (fun q st -> reverso q [1; 2; 3; 4] st);
  run "reverso"  int_list 1  (fun q st -> reverso [1; 2; 3; 4] q st); 
  run "just_a"   show_int 1  (fun q st -> just_a q st);
  run "a_and_b"  show_int 1  (fun q st -> a_and_b q st);
  run "a_and_b'" show_int 2  (fun q st -> a_and_b' q st);
  run "fives"    show_int 10 (fun q st -> fives q st)
