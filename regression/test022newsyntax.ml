open OCanren
open OCanren.Std

(* let f n = Nat.(<=) 0 n *)

let _ =
  Stdlib.List.iter (fun n -> print_endline (GT.show Std.Nat.ground n)) @@
    Stream.take ~n:1 @@
      run q (fun n ->
          ocanren {  Nat.(<=) 0 n }) project
