open MiniKanren
open Tester

let f1f state = print_endline "f1"; failure state
let f2f state = print_endline "f2"; failure state
let f3f state = print_endline "f3"; failure state

let f1s state = print_endline "f1"; success state
let f2s state = print_endline "f2"; success state
let f3s state = print_endline "f3"; success state

let () =
  run_exn string_of_int (-1) q qh (REPR(fun q -> (q=== inj_int 1) &&& conde  [ f1f; f2f; f3f ]));
  run_exn string_of_int (-1) q qh (REPR(fun q -> (q=== inj_int 1) &&& condel [ f1f; f2f; f3f ]));
  run_exn string_of_int (-1) q qh (REPR(fun q -> (q=== inj_int 1) &&& ?&     [ f1s; f2s; f3s ]));
  ()
