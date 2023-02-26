  $ ./main.exe
  fun q -> q === __, all answers {
  q=_.11;
  }
  fun q -> q =/= __, all answers {
  }
  fun _ -> __ =/= __, all answers {
  }
  fun _ -> (!! 5) =/= __, all answers {
  }
  fun _ -> (pair (!! 2) __) =/= (pair __ (!! 2)), all answers {
  }
  fun q -> fresh () (q =/= (pair __ (!! 1))) (q === (pair (!! 1) __)), all answers {
  q=(1, _.11 [=/= 1]);
  }
  fun _ -> (pair (!! 1) __) === (pair __ (!! 1)), all answers {
  q=_.10;
  }
  fun q -> (q === (pair __ (!! 1))) &&& (q === (pair (!! 1) __)), all answers {
  q=(1, 1);
  }
  fun _ -> (pair (!! 1) __) =/= (pair __ (!! 1)), all answers {
  }
  fun _ -> (pair (!! 1) __) =/= (pair (!! 2) __), all answers {
  q=_.10;
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
  fun q ->
    fresh (a b) (q === (pair a b)) (q =/= (pair (!! 1) __))
      (q === (pair __ (!! 1))), all answers {
  q=(_.13 [=/= 1], 1);
  }
  fun q -> fresh (a b) (q =/= (pair (!! 1) __)) (q === (pair a b)), all answers {
  q=(_.11 [=/= 1], _.12);
  }
  fun q -> fresh () (q =/= (pair (!! 1) __)) (q =/= (pair __ (!! 1))), all answers {
  q=_.10 [=/= (1, _.-42)];
  }
  fun q ->
    fresh (a b) (q =/= (pair (!! 1) __)) (q === (pair __ (!! 1)))
      (q === (pair a b)), all answers {
  q=(_.11 [=/= 1], 1);
  }
  fun q -> fresh () (q === ((!! 1) % ((!! 2) % __))) (q === (__ % __)), all answers {
  q=[1; 2 | _.11];
  }
  fun q -> fresh (a b) (q === ((!! 1) % ((!! 2) % __))) (q === (a % b)), all answers {
  q=[1; 2 | _.13];
  }
  fun q -> fresh () (q === (__ % __)) (q === (!< (!! 1))) (q === (!< (!! 2))), all answers {
  }
  fun q -> (q === (!! 1)) &&& (__ =/= ((!! 1) % __)), all answers {
  }
  fun q -> fresh (a b) (q === (!! 1)) (a =/= ((!! 1) % b)), all answers {
  q=1;
  }
  fun q -> fresh a (q === (!! 1)) (a =/= ((!! 1) % a)), all answers {
  q=1;
  }
  fun _ -> non_membero (!! 0) (Std.list (!!) [1; 2; 3]), all answers {
  q=_.10;
  }
  fun _ -> non_membero (!! 0) (Std.list (!!) []), all answers {
  q=_.10;
  }
  fun _ -> non_membero (!! 0) (Std.list (!!) [0]), all answers {
  }
  fun q -> (q =/= (__ % __)) &&& (q =/= (List.nil ())), all answers {
  q=_.10 [=/= [_.-42 | _.-42]; =/= []];
  }
  fun q -> (q =/= (Std.pair (!! true) __)) &&& (q === (Std.pair __ (!! true))), all answers {
  q=(_.11 [=/= true], true);
  }
  fun _ -> __ =/= (Std.pair __ (!! true)), all answers {
  }
  fun q ->
    fresh () (q === (Std.pair (!! false) (!! true)))
      (q =/= (Std.pair (!! true) __)), all answers {
  q=(false, true);
  }
  fun q ->
    fresh () (q =/= (Std.pair (!! true) __))
      (q === (Std.pair (!! false) (!! true))), all answers {
  q=(false, true);
  }
  fun _ ->
    fresh (x y) ((!! [x; !! 1]) =/= (!! [!! 2; y])) (y === (!! 1)) success, all answers {
  q=_.10;
  }
  fun q ->
    fresh (x y) ((Std.pair x (!! 1)) =/= (Std.pair (!! 2) y)) (x === (!! 2))
      (y === (!! 9)) ((Std.pair x y) === q), all answers {
  q=(2, 9);
  }
  fun q -> fresh (a b) (q === (pair a b)) (q =/= (pair (!! 1) __)), all answers {
  q=(_.11 [=/= 1], _.12);
  }
  fun q ->
    fresh (a b) (q === (pair a b)) (q =/= (pair (!! 1) __))
      (q === (pair __ (!! 1))), all answers {
  q=(_.13 [=/= 1], 1);
  }
  fun q -> fresh (a b) (q === (pair a b)) (q =/= (pair __ __)), all answers {
  }
  fun q ->
    fresh (a b c) (q === (Expr.make (!! "triple") (a % (b %< c))))
      (q =/= (Expr.make __ __)), all answers {
  }
  fun _ ->
    fresh www
      ((Std.pair (le (!! "one") (!! "x")) (le (!! "one") (!! "x"))) =/=
         (Std.pair (le __ www) (le __ www))), all answers {
  q=_.10;
  }
