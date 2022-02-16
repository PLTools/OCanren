  $ ./test017tabling.exe
  fun q -> patho arco1 !!"a" q, 10 answers {
  q=b;
  q=a;
  q=d;
  q=b;
  q=a;
  q=d;
  q=b;
  q=a;
  q=d;
  q=b;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=b;
  q=a;
  q=d;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=d;
  q=a;
  q=b;
  }
  fun q -> (q =/= !!"b") &&& patho arco1 !!"a" q, 10 answers {
  q=a;
  q=d;
  q=a;
  q=d;
  q=a;
  q=d;
  q=a;
  q=d;
  q=a;
  q=d;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=b;
  q=a;
  q=d;
  }
  fun q -> (q =/= !!"b") &&& patho_tabled !!"a" q, 10 answers {
  q=d;
  q=a;
  }
  fun q -> (q =/= !!"b") &&& patho arco1 !!"a" q, 10 answers {
  q=a;
  q=d;
  q=a;
  q=d;
  q=a;
  q=d;
  q=a;
  q=d;
  q=a;
  q=d;
  }
  fun q -> (q =/= !!"b") &&& patho_tabled !!"a" q, 10 answers {
  q=a;
  q=d;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=d;
  q=a;
  q=b;
  }
  fun q -> patho arco2 !!"a" q, 10 answers {
  q=_.11;
  q=a;
  q=_.14;
  q=_.15;
  q=a;
  q=a;
  q=_.20;
  q=_.21;
  q=_.22;
  q=_.23;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=_.11;
  q=a;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=a;
  q=_.11;
  }
  fun q -> patho arco3 !!"a" q, 10 answers {
  q=_.10 [=/= a; =/= b];
  q=a;
  q=_.10 [=/= a; =/= b];
  q=a;
  q=_.10 [=/= a; =/= b];
  q=a;
  q=_.10 [=/= a; =/= b];
  q=a;
  q=_.10 [=/= a; =/= b];
  q=a;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=_.10 [=/= a; =/= b];
  q=a;
  }
  fun q -> patho_tabled !!"a" q, 10 answers {
  q=a;
  q=_.11 [=/= a; =/= b];
  }
