  $ ./test001.exe
  fun q -> q === !!1 % q, 1 answer {
  }
  fun q -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4]), 1 answer {
  q=[1; 2];
  }
  fun q -> reverso q (ilist [1; 2; 3; 4]), 1 answer {
  q=[4; 3; 2; 1];
  }
  fun q -> reverso (ilist [1; 2; 3; 4]) q, 1 answer {
  q=[4; 3; 2; 1];
  }
  fun q -> reverso q (ilist [1]), 2 answers {
  q=[1];
  }
  fun q -> reverso (ilist [1]) q, 1 answer {
  q=[1];
  }
  fun q -> occurs q, 1 answer {
  }
  fun q -> a_and_b q, 1 answer {
  q=7;
  }
  fun q -> a_and_b' q, 2 answers {
  q=6;
  q=5;
  }
  fun q -> fives q, 10 answers {
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  q=5;
  }
  fun q -> success, 1 answer {
  q=_.10;
  }
  fun q -> OCanren.Fresh.one (fun n -> delay (fun () -> q === n % Std.nil ())), 1 answer {
  q=[_.11];
  }
  fun q -> OCanren.Fresh.three (fun a b c -> delay (fun () -> q === a % b % c)), 1 answer {
  q=[[_.11 | _.12] | _.13];
  }
  fun q -> reverso (ilist []) (ilist []), 1 answer {
  q=_.10;
  }
  fun q -> reverso q q, 2 answers {
  q=[];
  q=[_.11];
  }
  fun q r -> appendo q (ilist []) r, 4 answers {
  q=[]; r=[];
  q=[_.12]; r=[_.12];
  q=[_.12; _.15]; r=[_.12; _.15];
  q=[_.12; _.15; _.18]; r=[_.12; _.15; _.18];
  }
  fun q -> reverso q q, 1 answer {
  q=[];
  }
  fun q -> reverso q q, 2 answers {
  q=[];
  q=[_.11];
  }
  fun q -> reverso q q, 3 answers {
  q=[];
  q=[_.11];
  q=[_.11; _.11];
  }
  fun q -> reverso q q, 10 answers {
  q=[];
  q=[_.11];
  q=[_.11; _.11];
  q=[_.11; _.20; _.11];
  q=[_.11; _.20; _.20; _.11];
  q=[_.11; _.20; _.26; _.20; _.11];
  q=[_.11; _.20; _.26; _.26; _.20; _.11];
  q=[_.11; _.20; _.26; _.38; _.26; _.20; _.11];
  q=[_.11; _.20; _.26; _.38; _.38; _.26; _.20; _.11];
  q=[_.11; _.20; _.26; _.38; _.53; _.38; _.26; _.20; _.11];
  }
  fun q r -> two_vars q r, 1 answer {
  q=_.10; r=_.10;
  }
