(** {1 Useful functions to test running relational queries } *)

open Printf
open OCanren

(** {3 Helper functions to provide names for top-level variables } *)

(* let qh    = fun qs          -> ["q", qs] *)
(* let qrh   = fun qs rs       -> ["q", qs; "r", rs] *)

let wrap onOK onFree i (name, x) =
  if x#is_open
  then onFree i name x
  else onOK i name x

let qh onOK onFree = fun q () ->
  List.iteri (wrap onOK onFree) @@ ["q", q]

let qrh onOK onFree = fun q r () ->
  List.iteri (wrap onOK onFree) @@ ["q", q; "r", r]

let qrsh onOK onFree = fun q r s () ->
  List.iteri (wrap onOK onFree) @@ ["q", q; "r", r; "s", s]

let qrsth onOK onFree = fun q r s t () ->
  List.iteri (wrap onOK onFree) @@ ["q", q; "r", r; "s", s; "t", t]

let make_title n msg =
  printf "%s, %s answer%s {\n%!"
    msg
    (if n = (-1) then "all" else string_of_int n)
    (if n <>  1  then "s" else "")

exception NoMoreAnswers

let run_gen onOK onFree n num handler (repr, goal) =
  make_title n repr;
  let rec loop st = function
  | k when (k > n) && (n >= 0) -> ()
  | k ->
    match Stream.retrieve ~n:1 st with
    | [],_ -> raise NoMoreAnswers
    | [f],tl ->
      f ();
      printf "\n%!";
      loop tl (k+1)
    | _ -> assert false
  in
  let handler = handler onOK onFree in
  let () = try loop (run num goal handler) 1 with NoMoreAnswers -> () in
  printf "}\n%!"

(**
  [run_exn printer n name_helper goal] prints answers supposing there are no free variables there
  (i.e. reification is not required)
*)
let run_exn printer = run_gen
  (fun i name x -> printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printer x#prj) )
  (fun _ _ _ -> failwith "Free logic variables in the answer")

(**
  [runR reifier print_plain print_injected n name_helper goal] prints answers both with free varibles and
  without them. In the first cases it uses [print_plain] as printer fuction. In the latter case it does
  reification using [reifier] and prints the result wit [print_ibjected]
*)
let runR reifier printerNoFree printerR = run_gen
  (fun i name x -> printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printerNoFree x#prj) )
  (fun i name func ->
    let ans = func#reify reifier in
    printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printerR ans)
    )

let run_prjc reifier printer = run_gen
  (fun i name x ->
     printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printer x#prj) )
  (fun i name func ->
    let ans = func#prjc reifier in
    printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printer ans)
  )
