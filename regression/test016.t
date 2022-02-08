  $ ./test016sorto.exe
  fun q -> Nat.leo (nat 1) (nat 2) q, all answers {
  q=true;
  }
  fun q -> Nat.leo (nat 2) (nat 1) q, all answers {
  q=false;
  }
  fun q -> Nat.gto (nat 1) (nat 2) q, all answers {
  q=false;
  }
  fun q -> Nat.gto (nat 2) (nat 1) q, all answers {
  q=true;
  }
  fun q r -> minmaxo (nat 1) (nat 2) q r, all answers {
  q=S (O); r=S (S (O));
  }
