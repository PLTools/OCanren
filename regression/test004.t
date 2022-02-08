  $ ./test004.exe
  fun q -> addo o (s o) q, 1 answer {
  q=S (O);
  }
  fun q -> addo (s o) (s o) q, 1 answer {
  q=S (S (O));
  }
  fun q -> addo o (s o) q, 2 answers {
  q=S (O);
  }
  fun q -> addo (s o) (s o) q, 2 answers {
  q=S (S (O));
  }
  fun q -> addo q (s o) (s o), 1 answer {
  q=O;
  }
  fun q -> addo (s o) q (s o), 1 answer {
  q=O;
  }
  fun q -> addo q (s o) (s o), 2 answers {
  q=O;
  }
  fun q -> addo (s o) q (s o), 2 answers {
  q=O;
  }
  fun q r -> addo q r (s (s (s (s o)))), all answers {
  q=O; r=S (S (S (S (O))));
  q=S (O); r=S (S (S (O)));
  q=S (S (O)); r=S (S (O));
  q=S (S (S (O))); r=S (O);
  q=S (S (S (S (O)))); r=O;
  }
  fun q -> mulo o (s o) q, 1 answer {
  q=O;
  }
  fun q -> mulo (s (s o)) (s (s o)) q, 1 answer {
  q=S (S (S (S (O))));
  }
  fun q -> mulo o (s o) q, 2 answers {
  q=O;
  }
  fun q -> mulo q (s (s o)) (s (s o)), 1 answer {
  q=S (O);
  }
  fun q -> mulo q (s (s o)) (s (s (s o))), 1 answer {
  }
  fun q -> mulo q (s (s o)) (s (s o)), 2 answers {
  q=S (O);
  }
  fun q -> mulo q (s (s o)) (s (s (s o))), 2 answers {
  }
  fun q -> mulo (s (s o)) q (s (s o)), 1 answer {
  q=S (O);
  }
  fun q -> mulo (s (s o)) q (s (s (s o))), 1 answer {
  }
  fun q -> mulo (s (s o)) q (s (s o)), 2 answers {
  q=S (O);
  }
  fun q -> mulo (s (s o)) q (s (s (s o))), 2 answers {
  }
  fun q r -> mulo q (s o) r, 1 answer {
  q=O; r=O;
  }
  fun q r -> mulo q (s o) r, 10 answers {
  q=O; r=O;
  q=S (O); r=S (O);
  q=S (S (O)); r=S (S (O));
  q=S (S (S (O))); r=S (S (S (O)));
  q=S (S (S (S (O)))); r=S (S (S (S (O))));
  q=S (S (S (S (S (O))))); r=S (S (S (S (S (O)))));
  q=S (S (S (S (S (S (O)))))); r=S (S (S (S (S (S (O))))));
  q=S (S (S (S (S (S (S (O))))))); r=S (S (S (S (S (S (S (O)))))));
  q=S (S (S (S (S (S (S (S (O)))))))); r=S (S (S (S (S (S (S (S (O))))))));
  q=S (S (S (S (S (S (S (S (S (O))))))))); r=S (S (S (S (S (S (S (S (S (O)))))))));
  }
  fun q r -> mulo (s o) q r, 1 answer {
  q=O; r=O;
  }
  fun q r -> mulo (s o) q r, 10 answers {
  q=O; r=O;
  q=S (O); r=S (O);
  q=S (S (O)); r=S (S (O));
  q=S (S (S (O))); r=S (S (S (O)));
  q=S (S (S (S (O)))); r=S (S (S (S (O))));
  q=S (S (S (S (S (O))))); r=S (S (S (S (S (O)))));
  q=S (S (S (S (S (S (O)))))); r=S (S (S (S (S (S (O))))));
  q=S (S (S (S (S (S (S (O))))))); r=S (S (S (S (S (S (S (O)))))));
  q=S (S (S (S (S (S (S (S (O)))))))); r=S (S (S (S (S (S (S (S (O))))))));
  q=S (S (S (S (S (S (S (S (S (O))))))))); r=S (S (S (S (S (S (S (S (S (O)))))))));
  }
  fun q r -> mulo q r (s o), 1 answer {
  q=S (O); r=S (O);
  }
  fun q -> mulo (s o) (s o) q, 1 answer {
  q=S (O);
  }
  fun q r -> mulo q r (s (s (s (s o)))), 1 answer {
  q=S (S (S (S (O)))); r=S (O);
  }
  fun q r -> mulo q r (s (s (s (s o)))), 3 answers {
  q=S (S (S (S (O)))); r=S (O);
  q=S (S (O)); r=S (S (O));
  q=S (O); r=S (S (S (S (O))));
  }
  fun q r -> mulo q r o, 1 answer {
  q=O; r=_.11;
  }
  fun p q r -> mulo p q r, 3 answers {
  q=O; r=_.11; s=O;
  q=S (O); r=O; s=O;
  q=S (O); r=S (O); s=S (O);
  }
  fun q r s -> mulo q r s, 10 answers {
  q=O; r=_.11; s=O;
  q=S (O); r=O; s=O;
  q=S (O); r=S (O); s=S (O);
  q=S (S (O)); r=O; s=O;
  q=S (S (S (O))); r=O; s=O;
  q=S (O); r=S (S (O)); s=S (S (O));
  q=S (S (S (S (O)))); r=O; s=O;
  q=S (S (O)); r=S (O); s=S (S (O));
  q=S (S (S (S (S (O))))); r=O; s=O;
  q=S (S (S (S (S (S (O)))))); r=O; s=O;
  }
