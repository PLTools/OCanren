  $ ./test010.exe
  fun q -> g123 q, 3 answers {
  q=1;
  q=2;
  q=3;
  }
  fun q -> g123 q, 3 answers {
  q=1;
  q=2;
  q=3;
  }
  fun q -> g12 q, 3 answers {
  q=1;
  q=2;
  }
  fun q r -> gxy q r, 10 answers {
  q=1; r=1;
  q=1; r=2;
  q=2; r=1;
  q=1; r=3;
  q=3; r=1;
  q=2; r=2;
  q=3; r=2;
  q=2; r=3;
  q=3; r=3;
  }
  fun q r -> gxy' q r, 10 answers {
  q=1; r=2;
  q=2; r=1;
  q=1; r=3;
  q=3; r=1;
  q=3; r=2;
  q=2; r=3;
  }
  fun q ->
    OCanren.Fresh.two (fun x y -> delay (fun () -> conj (x === y) (x =/= y))), all answers {
  }
  fun q ->
    OCanren.Fresh.two (fun x y -> delay (fun () -> conj (x =/= y) (x === y))), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (conj (x =/= y) (!3 === x)) (!3 === y))), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (conj (!3 === x) (x =/= x)) (!3 === y))), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (conj (!3 === x) (!3 === y)) (x =/= y))), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (conj (!3 === x) (!3 === y)) (y =/= x))), all answers {
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (conj (x === y) (y === z)) (x =/= !4))
                (z === !(2 + 2)))), all answers {
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (conj (x === y) (y === z)) (z === !(2 + 2)))
                (x =/= !4))), all answers {
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (conj (x =/= !4) (y === z)) (x === y))
                (z === !(2 + 2)))), all answers {
  }
  fun q -> q =/= !5, all answers {
  q=_.10 [=/= 5];
  }
  fun q -> (q =/= !3) &&& (q === !3), all answers {
  }
  fun q -> (q === !3) &&& (!3 =/= q), all answers {
  }
  fun q r -> (q =/= !!(true)) &&& (q =/= r), all answers {
  q=_.10 [=/= _.11; =/= true]; r=_.11;
  }
  fun q -> q =/= Std.nil (), all answers {
  q=_.10 [=/= []];
  }
  fun q -> q =/= !<(!!2), all answers {
  q=_.10 [=/= [2]];
  }
