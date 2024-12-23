open OCanren
open Tester

let debug_line line =
  debug_var !!1 OCanren.reify (function _ ->
      Format.printf "%d\n%!" line;
      success)
;;

let trace_index msg var =
  debug_var var OCanren.reify (function
      | [ Var (n, _) ] ->
          Printf.printf "%s = _.%d\n" msg n;
          success
      | _ -> assert false)
;;

let trace fmt =
  Format.kasprintf
    (fun s ->
      debug_var !!1 OCanren.reify (function _ ->
          Format.printf "%s\n%!" s;
          success))
    fmt
;;

let rel list1 =
  let open OCanren.Std in
  fresh
    (list2 hd1 tl1 hd2 tl2)
    (trace_index "hd1" hd1)
    (trace_index "hd2" hd2)
    (trace_index "tl2" tl2)
    (list1 =/= list2)
    (list1 === hd1 % tl1)
    (list2 === hd2 % tl2)
    trace_diseq
    (trace " hd2 === 1")
    (hd2 === !!1)
    trace_diseq
    (trace " tl2 === []") (* bad behaviour starts now *)
    (tl2 === nil ())
    (hd1 === !!1)
    (debug_line __LINE__)
    trace_diseq
    (tl1 === nil ()) (* crashes here *)
    (debug_line __LINE__)
;;

(* let () = [%tester run_r [%show GT.int GT.list] (Std.List.reify reify) 1 (fun q -> rel q)] *)
let () = run_r (Std.List.reify reify) ([%show: GT.int logic Std.List.logic] ()) 1 q qh (REPR rel)
