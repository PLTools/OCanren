open Printf
open MiniKanren

let qh   = fun qs       -> ["q", qs]
let qrh  = fun qs rs    -> ["q", qs; "r", rs]
let qrsh = fun qs rs ss -> ["q", qs; "r", rs; "s", ss]

let make_title n msg =
  printf "%s, %s answer%s {\n%!"
    msg
    (if n = (-1) then "all" else string_of_int n)
    (if n <>  1  then "s" else "")

exception NoMoreAnswers

let run_gen onOK onFree n num (repr, goal) handler =
  make_title n repr;
  let rec loop pairs = function
  | 0 -> ()
  | k ->
    let new_pairs =
      List.map (fun (name,st) ->
        (* TODO: invent retrieve_hd function *)
        let (_: (_,'r) reification_rez Stream.t) = st in
        match Stream.retrieve ~n:1 st with
        | [],_ -> raise NoMoreAnswers
        | [Final x],tl ->
          onOK name ((Obj.magic x) : (_,'r) fancy);
          (name,tl)
        | [HasFreeVars (f,x)],tl ->
          onFree name f x;
          (name,tl)
        | _ -> assert false
      ) pairs
    in
    printf "\n%!";
    loop new_pairs (k-1)
  in
  let () = try loop (MiniKanren.run num goal handler) n with NoMoreAnswers -> () in
  printf "}\n%!"

(* Without free vars and reification *)
let run_exn printer = run_gen
  (fun name x -> printf "%s=%s; %!" name (printer x) )
  (fun _ _ _ -> failwith "Free logic variables in the answer")

let runR reifier printerNoFree printerR = run_gen
  (fun name x -> printf "%s=%s; %!" name (printerNoFree x) )
  (fun name isVar obj ->
    let ans = reifier isVar (fun () -> obj) in
    printf "%s=%s; %!" name (printerR ans)
    )

let (!!!) = Obj.magic
