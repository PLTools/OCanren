  $ ./test018prjc.exe
  fun q -> Fresh.one (fun r -> q === q), all answers {
  q=Var1 (10);
  }
  fun q -> Fresh.two (fun r s -> (r =/= s) &&& (q === X.a r)), all answers {
  q=A (Var2 (11));
  }
  fun q -> Fresh.one (fun r -> q === q), all answers {
  Fatal error: exception OCanren__Logic.Not_a_value
  [2]
