(** {1 Useful functions to test running relational queries } *)

open Printf
open OCanren

(** {3 Helper functions to provide names for top-level variables } *)

let wrap onOK i (name, x) =
  onOK i name x

let qh onOK = fun q () ->
  List.iteri (wrap onOK) @@ ["q", q]

let qrh onOK = fun q r () ->
  List.iteri (wrap onOK) @@ ["q", q; "r", r]

let qrsh onOK = fun q r s () ->
  List.iteri (wrap onOK) @@ ["q", q; "r", r; "s", s]

let qrsth onOK = fun q r s t () ->
  List.iteri (wrap onOK) @@ ["q", q; "r", r; "s", s; "t", t]

let make_title n msg =
  printf "%s, %s answer%s {\n%!"
    msg
    (if n = (-1) then "all" else string_of_int n)
    (if n <>  1  then "s" else "")

exception NoMoreAnswers

let run_gen onFree n num handler (repr, goal) =
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
  let handler = handler onFree in
  let () = try loop (run num goal handler) 1 with NoMoreAnswers -> () in
  printf "}\n%!"

let run_r reifier printerR = run_gen
  (fun i name (func : _ OCanren.reified) ->
    let ans = func#reify reifier in
    printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printerR ans)
    )
