  $ ./peano.exe
  fun q -> fresh v (q === (succ (succ v))), all answers {
  q=S (S (_.11));
  }
  fun q -> fresh () (le3 q) (q === (of_int 2)), all answers {
  q=S (S (O));
  }
  fun q -> fresh () (le3 q) (q === (of_int 3)), all answers {
  }
  fun q -> fresh () (le3 q) (q === (of_int 1)), all answers {
  q=S (O);
  }
  fun q -> fresh () (le3 q) (q === (of_int 0)), all answers {
  q=O;
  }
  fun q -> fresh () (le3 q) (q === (of_int 4)), all answers {
  }
