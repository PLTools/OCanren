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
  q=[_.23; _.23];
  }
  fun q -> reverso q q, 10 answers {
  q=[];
  q=[_.11];
  q=[_.23; _.23];
  q=[_.56; _.32; _.56];
  q=[_.110; _.89; _.89; _.110];
  q=[_.188; _.167; _.134; _.167; _.188];
  q=[_.293; _.266; _.236; _.236; _.266; _.293];
  q=[_.422; _.398; _.365; _.329; _.365; _.398; _.422];
  q=[_.578; _.554; _.524; _.488; _.488; _.524; _.554; _.578];
  q=[_.770; _.743; _.713; _.683; _.638; _.683; _.713; _.743; _.770];
  }
  fun q r -> two_vars q r, 1 answer {
  q=_.11; r=_.11;
  }
