  $ ./test013.exe
  fun q -> Bool.noto Bool.truo q, 1 answer {
  q=false;
  }
  fun q -> Bool.noto Bool.falso q, 1 answer {
  q=true;
  }
  fun q -> Bool.noto q Bool.truo, 1 answer {
  q=false;
  }
  fun q -> Bool.oro Bool.falso Bool.falso q, 1 answer {
  q=false;
  }
  fun q -> Bool.oro Bool.falso Bool.truo q, 1 answer {
  q=true;
  }
  fun q -> Bool.oro Bool.truo Bool.falso q, 1 answer {
  q=true;
  }
  fun q -> Bool.oro Bool.truo Bool.truo q, 1 answer {
  q=true;
  }
  fun q -> Bool.ando Bool.falso Bool.falso q, 1 answer {
  q=false;
  }
  fun q -> Bool.ando Bool.falso Bool.truo q, 1 answer {
  q=false;
  }
  fun q -> Bool.ando Bool.truo Bool.falso q, 1 answer {
  q=false;
  }
  fun q -> Bool.ando Bool.truo Bool.truo q, 1 answer {
  q=true;
  }
  fun q -> Nat.addo ?$0 ?$1 q, 1 answer {
  q=S (O);
  }
  fun q -> Nat.addo ?$1 q ?$3, 1 answer {
  q=S (S (O));
  }
  fun q r -> Nat.addo q r q, 3 answers {
  q=O; r=O;
  q=S (O); r=O;
  q=S (S (O)); r=O;
  }
  fun q -> Nat.mulo ?$1 ?$2 q, 1 answer {
  q=S (S (O));
  }
  fun q -> Nat.mulo ?$3 q ?$6, 1 answer {
  q=S (S (O));
  }
  fun q -> Nat.mulo ?$3 q ?$6, 1 answer {
  q=S (S (O));
  }
  fun q -> Nat.mulo ?$3 ?$0 q, 1 answer {
  q=O;
  }
  fun q -> Nat.mulo q ?$5 ?$0, 1 answer {
  q=O;
  }
  fun q -> Nat.mulo q ?$0 ?$0, 3 answers {
  q=O;
  q=S (O);
  q=S (S (O));
  }
  fun q -> sumo (nats []) q, 1 answer {
  q=O;
  }
  fun q -> sumo (nats [3; 1; 2]) q, 1 answer {
  q=S (S (S (S (S (S (O))))));
  }
  fun q -> sumo (?$0 % (?$1 % (q %< ?$3))) ?$6, 1 answer {
  q=S (S (O));
  }
  fun q -> List.lengtho (nats [1; 2; 3; 4]) q, 1 answer {
  q=S (S (S (S (O))));
  }
  fun q -> List.lengtho (list (!!) [(); (); ()]) q, 1 answer {
  q=S (S (S (O)));
  }
  fun q -> List.lengtho (bools [false; true]) q, 1 answer {
  q=S (S (O));
  }
  fun q -> List.lengtho (nats [4; 3; 2; 1; 0]) q, 1 answer {
  q=S (S (S (S (S (O)))));
  }
  fun q -> List.lengtho q ?$0, 1 answer {
  q=[];
  }
  fun q -> List.anyo (bools [false; false; true]) q, 1 answer {
  q=true;
  }
  fun q -> List.anyo (bools [false; false]) q, 1 answer {
  q=false;
  }
  fun q -> List.allo (bools [true; false; true]) q, 1 answer {
  q=false;
  }
  fun q -> List.allo (Bool.truo % (q %< Bool.truo)) Bool.truo, 1 answer {
  q=true;
  }
  fun q r s -> List.allo (Bool.truo % (q %< r)) s, all answers {
  q=true; r=true; s=true;
  q=false; r=false; s=false;
  q=false; r=true; s=false;
  q=true; r=false; s=false;
  }
  fun q -> List.mapo (Nat.addo ?$1) (nats [0; 1; 2]) q, 1 answer {
  q=[S (O); S (S (O)); S (S (S (O)))];
  }
  fun q -> List.mapo (Nat.addo ?$2) q (nats [4; 3; 2]), 1 answer {
  q=[S (S (O)); S (O); O];
  }
  fun q -> List.mapo (Nat.addo q) (nats [1; 2; 3]) (nats [4; 5; 6]), 1 answer {
  q=S (S (S (O)));
  }
  fun q -> List.mapo (Nat.mulo q) (nats [1; 2; 3]) (nats [2; 4; 6]), 1 answer {
  q=S (S (O));
  }
  fun q r -> List.mapo (Nat.mulo q) (nats [1; 2]) (?$2 %< r), 1 answer {
  q=S (S (O)); r=S (S (S (S (O))));
  }
  fun q -> List.mapo (===) (nats [1; 2; 3]) q, 1 answer {
  q=[S (O); S (S (O)); S (S (S (O)))];
  }
  fun q -> List.mapo (===) (nats [1; 2; 3]) (?$1 % (?$2 %< q)), 1 answer {
  q=S (S (S (O)));
  }
  fun q -> List.mapo Bool.noto (bools [true; false; true]) q, 1 answer {
  q=[false; true; false];
  }
  fun q -> List.mapo Bool.noto (bools []) q, 1 answer {
  q=[];
  }
  fun q -> List.filtero (eqo ?$2) (nats [0; 1; 2; 3]) q, all answers {
  q=[S (S (O))];
  }
  fun q -> List.lookupo (eqo ?$1) (nats [0; 2; 1; 3]) q, 1 answer {
  q=Some (S (O));
  }
  fun q r -> Nat.mulo q r q, 3 answers {
  q=O; r=_.11;
  q=S (O); r=S (O);
  q=S (S (O)); r=S (O);
  }
  fun q -> List.lengtho q ?$3, all answers {
  q=[_.11; _.14; _.17];
  }
