  $ ./test006.exe
  fun q -> substo (v varX) varX (v varY) q, 1 answer {
  q=V ("y");
  }
  fun q -> evalo (abs varX (v varX)) q, 1 answer {
  q=Abs ("x", V ("x"));
  }
  fun q -> evalo (abs varX (v varX)) q, 2 answers {
  q=Abs ("x", V ("x"));
  }
  fun q -> evalo (app (abs varX (v varX)) (v varY)) q, 1 answer {
  q=V ("y");
  }
  fun q -> evalo (app (abs varX (v varX)) q) (v varY), 1 answer {
  q=V ("y");
  }
  fun q -> evalo (app (abs varX q) (v varY)) (v varY), 1 answer {
  q=V ("x");
  }
  fun q -> evalo (app (v varX) (v varX)) q, 1 answer {
  q=App (V ("x"), V ("x"));
  }
  fun q -> evalo (v varX) q, 1 answer {
  q=V ("x");
  }
  fun q -> evalo (app q (v varX)) (v varX), 1 answer {
  q=Abs (_.9, V (_.9));
  }
  fun q r -> evalo (app r q) (v varX), 1 answer {
  q=V ("x"); r=Abs (_.10, V (_.10));
  }
  fun q r s -> a_la_quine q r s, 2 answers {
  q=Abs (_.11, V (_.11)); r=Abs (_.11, V (_.11)); s=Abs (_.11, V (_.11));
  q=Abs (_.11, V (_.11)); r=Abs (_.11, Abs (_.11, V (_.11))); s=Abs (_.11, Abs (_.11, V (_.11)));
  }
