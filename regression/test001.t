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
  q=_.0;
  }
  fun q -> OCanren.Fresh.one (fun n -> delay (fun () -> q === n % Std.nil ())), 1 answer {
  q=[_.1];
  }
  fun q -> OCanren.Fresh.three (fun a b c -> delay (fun () -> q === a % b % c)), 1 answer {
  q=[[_.1 | _.2] | _.3];
  }
  fun q -> reverso (ilist []) (ilist []), 1 answer {
  q=_.0;
  }
  fun q -> reverso q q, 2 answers {
  q=[];
  q=[_.1];
  }
  fun q r -> appendo q (ilist []) r, 4 answers {
  q=[]; r=[];
  q=[_.2]; r=[_.2];
  q=[_.2; _.5]; r=[_.2; _.5];
  q=[_.2; _.5; _.8]; r=[_.2; _.5; _.8];
  }
  fun q -> reverso q q, 1 answer {
  q=[];
  }
  fun q -> reverso q q, 2 answers {
  q=[];
  q=[_.1];
  }
  fun q -> reverso q q, 3 answers {
  q=[];
  q=[_.1];
  q=[_.1; _.1];
  }
  fun q -> reverso q q, 10 answers {
  q=[];
  q=[_.1];
  q=[_.1; _.1];
  q=[_.1; _.10; _.1];
  q=[_.1; _.10; _.10; _.1];
  q=[_.1; _.10; _.16; _.10; _.1];
  q=[_.1; _.10; _.16; _.16; _.10; _.1];
  q=[_.1; _.10; _.16; _.25; _.16; _.10; _.1];
  q=[_.1; _.10; _.16; _.25; _.25; _.16; _.10; _.1];
  q=[_.1; _.10; _.16; _.25; _.43; _.25; _.16; _.10; _.1];
  }
  fun q r -> two_vars q r, 1 answer {
  q=_.0; r=_.0;
  }
