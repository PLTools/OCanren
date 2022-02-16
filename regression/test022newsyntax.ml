open OCanren
open OCanren.Std

let _ =
  Stdlib.List.iter (fun n -> print_endline (GT.show Std.Nat.ground n)) @@
    Stream.take ~n:1 @@
      run q (fun n ->
          ocanren {fresh c     in Nat.(<=) 0 n
                                & Nat.(<=) n 0
                                & n == Nat.zero })
        (fun rr -> rr#reify Nat.prj_exn)
