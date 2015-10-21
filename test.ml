open GT
open MiniKanren
open Printf

(* @type t = A of int | B of string | C of t * t with show, minikanren  *)
let (|>) x f = f x

let () =
  (* let (>>=) m f = match m with Some x -> Some (f x) | None -> None in *)
  (* let iter m ~f = *)
  (*   print_endline ""; *)
  (*   match m with Some x -> f x | None -> print_endline "bad" in *)
  (* print_endline "testing unification"; *)
  (* let var10, e   = Env.fresh (Env.empty ())  in *)
  (* let var11, e   = Env.fresh e  in *)
  (* let var12, e   = Env.fresh e  in *)
  (* let var13, e   = Env.fresh e  in *)
  (* let var14, e   = Env.fresh e  in *)
  (* let var15, e   = Env.fresh e  in *)
  (* let var16, e   = Env.fresh e  in *)
  (* let var17, e   = Env.fresh e  in *)

  (* let printer e x = *)
  (*   (\* print_endline "\nprinter"; *\) *)
  (*   sprintf "%s" (generic_show !!x) *)
  (*   (\* show_list e show_int *\) *)
  (* in *)

  (* let s = Some Subst.empty in *)
  (* let s = s |> Subst.unify e var10 (Obj.magic (var12::var13)) in *)
  (* let s = s |> Subst.unify e var11 (Obj.magic (var12::var14)) in *)
  (* let s = s |> Subst.unify e var13 (Obj.magic (var15::var16)) in *)
  (* let s = s |> Subst.unify e var16 (Obj.magic []) in *)
  (* let s = s |> Subst.unify e var14 (Obj.magic (var15::var17)) in *)
  (* let s = s |> Subst.unify e var17 (Obj.magic []) in *)

  (* iter s ~f:(fun subst -> *)
  (*     printf "subst = %s\n%!" (Subst.show subst); *)
  (*     printf "var10=%s, var11=%s\n%!" (printer e (Subst.walk' e var10 subst)) (printer e (Subst.walk' e var11 subst)) *)
  (*   ); *)

  ()


let run_2var memo printer n goal =
  let q, e   = Env.fresh (Env.empty ())  in
  let r, e   = Env.fresh e               in
  let st     = e, Subst.empty            in
  let result = Stream.take n (goal q r st) in
  Printf.printf "%s {\n" memo;
  List.iter
    (fun ((env, subst) as st) ->
        printf "State:\n"; flush stdout;
        printf "%s\n" (show_st st); flush stdout;
        printf "q=%d, r=%d\n" (let Var i = !! q in i) (let Var i = !!r in i);
        printf "q=%s, r=%s\n" (printer env (Subst.walk' env q subst)) (printer env (Subst.walk' env r subst))
    )
    result;
  Printf.printf "}\n%!"

(* copy and pase
   Maybe we should implement more polymorphic runner like Oleg Kiselev
*)
let run_1var memo printer n goal =
  let q, e   = Env.fresh (Env.empty ())  in
  let st     = e, Subst.empty            in
  let result = Stream.take n (goal q st) in
  Printf.printf "%s {\n" memo;
  List.iter
    (fun ((env, subst) as st) ->
        printf "State:\n"; flush stdout;
        printf "%s\n" (show_st st); flush stdout;
        printf "q=%d\n%!" (let Var i = !! q in i);
        printf "q='%s'\n" (printer env (Subst.walk' env q subst))
    )
    result;
  Printf.printf "}\n%!"


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

let int_list e l =
  printf "int_list: env=%s, list=%s\n%!"
     (Env.show e)
     (generic_show !!l);
  show_list e show_int l

let rec appendo a b ab ((env, subst) as st) =
  (* logn "show [] = '%s'" (generic_show !![]); *)
  (* logn "show 5  = '%s'" (generic_show !!5); *)
  (* logn "show [5] = '%s'" (generic_show !![5]); *)

  logn "appendo %s, %s, %s" (generic_show !!a) (generic_show !!b) (generic_show !!ab);
  if MiniKanren.config.do_readline then ignore (read_line ());

  disj
    (conj (a === []) (b === ab) )
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
  logn "reverso: %s %s" (generic_show !!a) (generic_show !!b);
  if MiniKanren.config.do_readline then ignore (read_line ());

  disj
    (conj (a === []) (b === []))
    (fresh (fun h ->
      (fresh (fun t ->
          (conj (a === h::t)
              (fresh (fun a' ->
                 conj (appendo a' [h] b)
                      (reverso t a')
              ))
        )
    )
  ))) st

(* let rec test_rev1 a b ((env, subst) as st) = *)
(*   fresh (fun h -> *)
(*     fresh (fun t -> *)
(*       disj *)
(*         (conj (a === []) (b === [])) *)
(*         (conj (a === h::t) *)
(*               (fresh (fun a' -> *)
(*                       (reverso t a') *)
(*               )) *)
(*         ) *)
(*      ) *)
(*   ) st *)

let rec rev_test1 f g (e,st) =
(*  reverso: boxed 0 <int<12>> boxed 0 <int<13>>
disj st {env {$10; $13; $11; $12; }, subst {10 -> boxed 0 <boxed 0 <int<11>> []>; 11 -> int<1>; 12 -> []; 13 -> []; }} *)
  let q, e   = Env.fresh e  in
  let r, e   = Env.fresh e               in
  let st = Subst.unify e q [] (Some st) in
  let st = Subst.unify e r [] st in
  match st with
    | None -> failwith "st is bad"
    | Some st -> reverso q r (e,st)






let _ =
  (* run "appendo" int_list 1 (fun q st -> appendo q [3; 4] [1; 2; 3; 4] st); *)
(*  run_2var "appendo q [] r max 1 result" int_list 1 (fun q r st -> appendo q [] r st); *)
(*   run_2var "appendo q [] r max 2 result" int_list 2 (fun q r st -> appendo q [] r st); *)
 (*  run_2var "appendo q [] r max 3 result" int_list 3 (fun q r st -> appendo q [] r st); *)
  (* run_2var "appendo q [] r max 4 result" int_list 4 (fun q r st -> appendo q [] r st); *)
(*   run_1var "reverso q [1] max 1 result" int_list 1 (fun q st -> reverso q [1; 2; 3; 4] st); *)
   run_1var "reverso [] [] max 1 result" int_list 1 (fun q st -> reverso [] [] st);
   run_1var "reverso [1] q max 1 result" int_list 1 (fun q st -> reverso [1; 2; 3; 4] q st);
   run_1var "rev_test1 max 1 result" int_list 1 (fun q st -> rev_test1 q q st);

  (* run_1var "reverso q q max 1  result" int_list 1  (fun q st -> reverso q q st); *)
  (* run_1var "reverso q q max 2  result" int_list 2  (fun q st -> reverso q q st); *)
  (* run_1var "reverso q q max 3  result" int_list 3  (fun q st -> reverso q q st); *)
  (* run_1var "reverso q q max 10 result" int_list 10 (fun q st -> reverso q q st); *)

  (* run_1var "reverso q [1] max 2 results" int_list 1 (fun q st -> reverso q [1] st); *)
  (* run_1 "reverso [1] 1 max 2 results" int_list 1 (fun q st -> reverso [1] q st); *)
  (*
  run "reverso"  int_list 1  (fun q st -> reverso [1; 2; 3; 4] q st);
  run "just_a"   show_int 1  (fun q st -> just_a q st);
  run "a_and_b"  show_int 1  (fun q st -> a_and_b q st);
  run "a_and_b'" show_int 2  (fun q st -> a_and_b' q st);
  run "fives"    show_int 10 (fun q st -> fives q st)*)
  ()
