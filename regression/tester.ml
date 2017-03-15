(** {1 Useful functions to test running relational queries } *)

open Printf
open MiniKanren

(** {3 Helper functions to provide names for top-level variables } *)

let qh    = fun qs          -> ["q", qs]
let qrh   = fun qs rs       -> ["q", qs; "r", rs]
let qrsh  = fun qs rs ss    -> ["q", qs; "r", rs; "s", ss]
let qrsth = fun qs rs ss ts -> ["q", qs; "r", rs; "s", ss; "t", ts]

let make_title n msg =
  printf "%s, %s answer%s {\n%!"
    msg
    (if n = (-1) then "all" else string_of_int n)
    (if n <>  1  then "s" else "")

exception NoMoreAnswers

let run_gen onOK onFree n num handler (repr, goal) =
  make_title n repr;
  let rec loop pairs = function
  | 0 -> ()
  | k ->
    let new_pairs =
      List.mapi (fun i (name,st) ->
        (* TODO: invent retrieve_hd function *)
        match Stream.retrieve ~n:1 st with
        | [],_ -> raise NoMoreAnswers
        | [rr],tl ->
          if rr#is_open
          then onFree i name (fun r -> rr#refine r ~inj:(fun _ -> assert false))
          else onOK i name rr#prj;
          (name,tl)
        | _ -> assert false
      ) pairs
    in
    printf "\n%!";
    loop new_pairs (k-1)
  in
  let () = try loop (MiniKanren.run num goal handler) n with NoMoreAnswers -> () in
  printf "}\n%!"

(**
  [run_exn printer n name_helper goal] prints answers supposing there are no free variables there
  (i.e. reification is not required)
*)
let run_exn printer = run_gen
  (fun i name x -> printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printer x) )
  (fun _ _ _ -> failwith "Free logic variables in the answer")

(**
  [runR reifier print_plain print_injected n name_helper goal] prints answers both with free varibles and
  without them. In the first cases it uses [print_plain] as printer fuction. In the latter case it does
  reification using [reifier] and prints the result wit [print_ibjected]
*)
let runR reifier printerNoFree printerR = run_gen
  (fun i name x -> printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printerNoFree x) )
  (fun i name func ->
    let ans = func reifier in
    printf "%s%s=%s;%!" (if i<>0 then " " else "") name (printerR ans)
    )
