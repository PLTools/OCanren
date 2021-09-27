  $ ./main.exe
  fun q -> q === __, all answers {
  q=_.11;
  }
  fun q -> q =/= __, all answers {
  }
  fun q -> (pair (!! 2) __) =/= (pair __ (!! 2)), all answers {
  }
  fun q -> fresh () (q =/= (pair __ (!! 1))) (q === (pair (!! 1) __)), all answers {
  q=(1, _.11 [=/= 1]);
  }
  fun q -> (pair (!! 1) __) === (pair __ (!! 1)), all answers {
  q=_.10;
  }
  fun q -> (q === (pair __ (!! 1))) &&& (q === (pair (!! 1) __)), all answers {
  q=(1, 1);
  }
  fun q -> (pair (!! 1) __) =/= (pair __ (!! 1)), all answers {
  }
  fun q -> (triple q (!! 2) __) =/= (triple (!! 1) __ (!! 2)), all answers {
  q=_.10 [=/= 1];
  }
  fun q -> fun r -> (pair q r) =/= (pair (!! 1) __), all answers {
  q=_.10 [=/= 1]; r=_.11;
  }
  fun q -> (pair q (!! 1)) =/= (pair (!! 1) __), all answers {
  q=_.10 [=/= 1];
  }
  fun q -> (pair q (!! 1)) =/= (pair (!! 1) __), all answers {
  q=_.10 [=/= 1];
  }
  fun q ->
    fresh (a b) (q === (pair a b)) (q =/= (pair (!! 1) __))
      (q === (pair __ (!! 1))), all answers {
  q=(_.13 [=/= 1], 1);
  }
  fun q ->
    fresh (a b) (q === (pair a b)) (q =/= (pair (!! 1) __))
      (q =/= (pair __ (!! 1))), all answers {
  q=(_.11 [=/= 1], _.12 [=/= 1]);
  }
  fun q -> fresh () (q =/= (pair (!! 1) __)) (q =/= (pair __ (!! 1))), all answers {
  q=_.10 [=/= (_.-42, 1); =/= (1, _.-42)];
  }
  fun q -> fresh __ (q =/= (pair (!! 1) __)) (q =/= (pair __ (!! 1))), all answers {
  q=_.10 [=/= (_.-42, 1); =/= (1, _.-42)];
  }
  fun q ->
    fresh (a b) (q =/= (pair (!! 1) __)) (q === (pair __ (!! 1)))
      (q === (pair a b)), all answers {
  q=(_.11 [=/= 1], 1);
  }
  fun q -> fresh () (q === ((!! 1) % ((!! 2) % __))) (q === (__ % __)), all answers {
  q=[1; 2; _.11];
  }
  fun q -> fresh (a b) (q === ((!! 1) % ((!! 2) % __))) (q === (a % b)), all answers {
  q=[1; 2; _.13];
  }
  fun q ->
    fresh (a b) (q === (__ % __)) (q === (!< (!! 1))) (q === (!< (!! 2))), all answers {
  }
  fun q -> (q === (!! 1)) &&& (__ =/= ((!! 1) % __)), all answers {
  }
  fun q -> fresh (a b) (q === (!! 1)) (a =/= ((!! 1) % b)), all answers {
  q=1;
  }
  fun q -> fresh a (q === (!! 1)) (a =/= ((!! 1) % a)), all answers {
  q=1;
  }
  fun q -> non_membero (!! 0) (Std.list (!!) [1; 2; 3]), all answers {
  q=_.10;
  }
  fun q -> non_membero (!! 0) (Std.list (!!) []), all answers {
  q=_.10;
  }
  fun q -> non_membero (!! 0) (Std.list (!!) [0]), all answers {
  }
  fun q -> (q =/= (__ % __)) &&& (q =/= (List.nil ())), all answers {
  q=_.10 [=/= [_.-42; _.-42]; =/= []];
  }
