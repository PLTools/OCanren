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

open Tester

let run eta = Tester.run_r (Std.List.reify OCanren.reify) (GT.show Std.List.logic @@ GT.show OCanren.logic (GT.show GT.int)) eta

let () =
  run (-1) q qh (REPR(fun q -> fresh (h tl) (q === h % tl)))

let () =
  run (-1) q qh (REPR(fun q -> ocanren { q == _ :: _ }))
