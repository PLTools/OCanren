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
  q=Abs (_.44, V (_.44));
  }
  fun q r -> evalo (app r q) (v varX), 1 answer {
  q=V ("x"); r=Abs (_.54, V (_.54));
  }
  fun q r s -> a_la_quine q r s, 2 answers {
  q=Abs (_.859, V (_.859)); r=Abs (_.859, V (_.859)); s=Abs (_.859, V (_.859));
  q=Abs (_.1192, V (_.1192)); r=Abs (_.1192, Abs (_.1192, V (_.1192))); s=Abs (_.1192, Abs (_.1192, V (_.1192)));
  }
