  $ ./test022newsyntax.exe
  O
  fun q -> OCanren.Fresh.two (fun h tl -> delay (fun () -> q === h % tl)), all answers {
  q=[_.1 | _.2];
  }
  fun q ->
    OCanren.Fresh.two
      (fun __1 __2 -> OCanren.unify q (OCanren.Std.List.cons __1 __2)), all answers {
  q=[_.1 | _.2];
  }
