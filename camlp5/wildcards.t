These are tests for OCanren-spefici `ocanren { ... }` syntax extension
  $ echo 'let _ = ocanren { fresh h, tl in Nat.(<=) 0 & (Nat.(<=) n 0) & n == Nat.zero}' > test0.ml
  $ camlp5o -I . pa_o.cmo pr_o.cmo pa_ocanren.cma test0.ml
  let _ =
    OCanren.Fresh.two
      (fun h tl ->
         OCanren.conj (Nat.(<=) (OCanren.Std.nat 0))
           (OCanren.conj (Nat.(<=) n (OCanren.Std.nat 0))
              (OCanren.unify n Nat.zero)))
  $ echo 'let _ = ocanren { fresh h, tl in  q == _ :: _ }' > test1.ml
  $ camlp5o -I . pa_o.cmo pr_o.cmo pa_ocanren.cma test1.ml
  let _ =
    OCanren.Fresh.two
      (fun h tl ->
         OCanren.Fresh.two
           (fun __1 __2 -> OCanren.unify q (OCanren.Std.List.cons __1 __2)))
  $ echo 'let _ = ocanren { fresh h in  q == [ h; _; _ ; _ ] }' > test2.ml
  $ camlp5o -I . pa_o.cmo pr_o.cmo pa_ocanren.cma test2.ml
  let _ =
    OCanren.Fresh.one
      (fun h ->
         OCanren.Fresh.three
           (fun __1 __2 __3 ->
              OCanren.unify q
                (OCanren.Std.List.cons h
                   (OCanren.Std.List.cons __1
                      (OCanren.Std.List.cons __2
                         (OCanren.Std.List.cons __3 (OCanren.Std.nil ())))))))
# expanding wildcards in standart miniKanren syntax extension is not yet implemented
  $ echo 'let __ q  =  q === (__ %__)' > test2.ml
  $ camlp5o -I . pa_o.cmo pr_o.cmo pa_ocanren.cma test2.ml
  let __ q = q === __ % __
