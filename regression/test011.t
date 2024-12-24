  $ ./test011.exe
  fun q -> q =/= !1, all answers {
  q=_.10 [=/= 1];
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (x =/= y) (x === !![!0; z; !1]))
                (y === !![!0; !1; !1]))), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj
                (conj (conj (x =/= y) (x === !![!0; z; !1]))
                   (y === !![!0; !1; !1]))
                (z === !0))), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (conj (z === !0) (x =/= y)) (x === !![!0; z; !1]))
                (y === !![!0; !1; !1]))), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (conj (z === !1) (x =/= y)) (x === !![!0; z; !1]))
                (y === !![!0; !1; !1]))), all answers {
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj (conj (x === !![!0; z; !1]) (y === !![!0; !1; !1]))
                (x =/= y))), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.three
      (fun x y z ->
         delay
           (fun () ->
              conj
                (conj (conj (z === !1) (x === !![!0; z; !1]))
                   (y === !![!0; !1; !1]))
                (x =/= y))), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (!![x; !1] =/= !![!2; y]) (x === !2))), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (!![x; !1] =/= !![!2; y]) (y === !1))), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay (fun () -> conj (!![x; !1] =/= !![!2; y]) (!![x; y] === q))), all answers {
  q=[_.11; _.12 [=/= 1]];
  q=[_.11 [=/= 2]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (!![x; !1] =/= !![!2; y]) (x === !2))
                (!![x; y] === q))), all answers {
  q=[2; _.12 [=/= 1]];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (conj (!![x; !1] =/= !![!2; y]) (x === !2)) (y === !9))
                (!![x; y] === q))), all answers {
  q=[2; 9];
  }
  fun q ->
    OCanren.Fresh.two
      (fun a d ->
         delay
           (fun () ->
              conj (conj (conj (!![a; d] === q) (q =/= !![!5; !6])) (a === !5))
                (d === !6))), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (conj (!![x; !1] =/= !![!2; y]) (x === !2)) (y === !1))
                (!![x; y] === q))), all answers {
  }
  fun q ->
    OCanren.Fresh.three
      (fun a x z ->
         delay
           (fun () ->
              conj (conj (a =/= !![x; !1]) (a === !![z; !1])) (x === z))), all answers {
  }
  fun x ->
    OCanren.Fresh.one (fun y -> delay (fun () -> !![x; y] =/= !![!5; !6])), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.three
      (fun a x z ->
         delay
           (fun () ->
              conj (conj (conj (a =/= !![x; !1]) (a === !![z; !1])) (x === !5))
                (!![x; z] === q))), all answers {
  q=[5; _.13 [=/= 5]];
  }
  fun q -> !3 =/= !4, all answers {
  q=_.10;
  }
  fun q -> !3 =/= !3, all answers {
  }
  fun q -> ((!5 =/= q) &&& (!6 =/= q)) &&& (q === !5), all answers {
  }
  fun q ->
    OCanren.Fresh.two
      (fun a d ->
         delay
           (fun () ->
              conj (conj (!![a; d] === q) (q =/= !![!5; !6])) (a === !5))), all answers {
  q=[5; _.12 [=/= 6]];
  }
  fun q ->
    OCanren.Fresh.one (fun a -> delay (fun () -> conj (!3 === a) (a =/= !4))), all answers {
  q=_.10;
  }
  fun q -> (!4 =/= q) &&& (!3 =/= q), all answers {
  q=_.10 [=/= 3; =/= 4];
  }
  fun q -> (q =/= !5) &&& (q =/= !5), all answers {
  q=_.10 [=/= 5];
  }
  fun q ->
    let foo x = OCanren.Fresh.one (fun a -> delay (fun () -> x =/= a)) in
    OCanren.Fresh.one (fun a -> delay (fun () -> foo a)), all answers {
  q=_.10;
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (!![x; y] === q) (x =/= y))), all answers {
  q=[_.11 [=/= _.12]; _.12];
  }
  fun q -> Fresh.two (fun a d -> ?&[!![a; d] === q; q =/= !![!5; !6]]), all answers {
  q=[_.11; _.12 [=/= 6]];
  q=[_.11 [=/= 5]; _.12];
  }
  fun q -> Fresh.two (fun a d -> ?&[!![a; d] === q; q =/= !![!5; !6]; a === !3]), all answers {
  q=[3; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y -> delay (fun () -> conj (!![x; y] === q) (y =/= x))), all answers {
  q=[_.11; _.12 [=/= _.11]];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay (fun () -> conj (conj (!![x; y] === q) (x =/= y)) (y =/= x))), all answers {
  q=[_.11 [=/= _.12]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay (fun () -> conj (conj (!![x; y] === q) (x =/= y)) (x =/= y))), all answers {
  q=[_.11 [=/= _.12]; _.12];
  }
  fun q -> (q =/= !5) &&& (!5 =/= q), all answers {
  q=_.10 [=/= 5];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (!![x; y] === q) (!![x; y] =/= !![!5; !6]))
                (x =/= !5))), all answers {
  q=[_.11 [=/= 5]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (!![x; y] === q) (x =/= !5))
                (!![x; y] =/= !![!5; !6]))), all answers {
  q=[_.11 [=/= 5]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (x =/= !5) (!![x; y] =/= !![!5; !6]))
                (!![x; y] === q))), all answers {
  q=[_.11 [=/= 5]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (!5 =/= x) (!![x; y] =/= !![!5; !6]))
                (!![x; y] === q))), all answers {
  q=[_.11 [=/= 5]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (!5 =/= x) (!![y; x] =/= !![!6; !5]))
                (!![x; y] === q))), all answers {
  q=[_.11 [=/= 5]; _.12];
  }
  fun q ->
    OCanren.Fresh.two
      (fun x y ->
         delay
           (fun () ->
              conj (conj (x % y === q) (x % y =/= !![!1; x])) (y === !![!2]))), all answers {
  q=[_.11; 2];
  }
  fun x ->
    OCanren.Fresh.two
      (fun y z -> delay (fun () -> conj (x =/= !![y; !2]) (x === !![z; !2]))), all answers {
  q=[_.12 [=/= _.11]; 2];
  }
  fun q -> distincto (!2 % (!3 %< q)), all answers {
  q=_.35 [=/= 2; =/= 3];
  }
  fun q -> remembero !1 (!1 % (!2 % (!1 %< !3))) q, all answers {
  q=[2; 3];
  q=[2; 1; 3];
  q=[1; 2; 3];
  q=[1; 2; 1; 3];
  }
  fun q -> remembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)), all answers {
  q=_.10;
  }
  fun q -> rembero !1 (!1 % (!2 % (!1 %< !3))) q, all answers {
  q=[2; 3];
  }
  fun q -> rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)), all answers {
  }
