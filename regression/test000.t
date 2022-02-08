  $ ./test000.exe
  fun q -> q === !!5, 1 answer {
  q=5;
  }
  fun q -> q === some !!5, 1 answer {
  q=Some (5);
  }
  fun q -> q === none (), 1 answer {
  q=None;
  }
  fun q -> some q === some !!5, 1 answer {
  q=5;
  }
  fun q -> OCanren.Fresh.one (fun w -> delay (fun () -> q === some w)), 1 answer {
  q=Some (_.11);
  }
  fun q -> q === Result.ok !!5, 1 answer {
  q=Ok (5);
  }
  fun q ->
    OCanren.Fresh.one
      (fun r ->
         delay
           (fun () -> conj (q === Result.ok r) (conde [r === !!5; success]))), all answers {
  q=Ok (5);
  q=Ok (_.11);
  }
  fun q ->
    OCanren.Fresh.two
      (fun r s ->
         delay
           (fun () ->
              conde
                [(q === Result.ok s) &&& (s =/= !!4); q === Result.error r])), all answers {
  q=Ok (_.12 [=/= 4]);
  q=Error (_.11);
  }
