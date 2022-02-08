  $ ./test009.exe
  fun q -> pExpr (list (!!) [Id]) q, 1 answer {
  q=I;
  }
  fun q -> pExpr (list (!!) [Id; Mul; Id]) q, 1 answer {
  q=M (I, I);
  }
  fun q -> pExpr (list (!!) [Id; Mul; Id; Mul; Id]) q, 1 answer {
  q=M (I, M (I, I));
  }
  fun q -> pExpr (list (!!) [Id; Mul; Id; Add; Id]) q, 1 answer {
  q=A (M (I, I), I);
  }
  fun q -> pExpr (list (!!) [Id; Add; Id; Mul; Id]) q, 1 answer {
  q=A (I, M (I, I));
  }
  fun q -> pExpr (list (!!) [Id; Add; Id; Add; Id]) q, 1 answer {
  q=A (I, A (I, I));
  }
  fun q -> pExpr q (m (i ()) (i ())), 1 answer {
  q=[Id; Mul; Id];
  }
