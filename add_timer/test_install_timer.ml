open OCanren
open Tester

let () =
  run_r (OCanren.prj_exn) (GT.show GT.int) (-1) q qh ("", fun q -> q === !!1)
