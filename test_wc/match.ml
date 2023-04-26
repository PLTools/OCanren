open OCanren
open Tester
open OCanren.Std

let bool_dom l = conde [ l === !!false; l === !!true ]
let run_int eta = run_r OCanren.reify ([%show: GT.int logic] ()) eta

module _ = struct
  let source = {|
    match ... with
    | t,_ -> 1
    | _,_ -> 2
  |}

  let%expect_test " " =
    Printf.printf "Pseudecode:\n%s\n" source;
    [%expect
      {|
      Pseudecode:

          match ... with
          | t,_ -> 1
          | _,_ -> 2 |}]
  ;;

  let run_m eta =
    run_r
      (Pair.reify (Pair.reify OCanren.reify OCanren.reify) OCanren.reify)
      ([%show:
         ((GT.bool logic, GT.bool logic) Std.Pair.logic, GT.int logic) Std.Pair.logic]
         ())
      eta
  ;;

  let test ?(explicit = true) rel =
    [%tester
      run_m (-1) (fun q ->
        fresh
          (scru rhs)
          (q === pair scru rhs)
          (rel scru rhs)
          (fresh
             (l r)
             (scru === Std.pair l r)
             (if explicit then bool_dom l &&& bool_dom r else success)))]
  ;;

  let naive_rel q rez =
    conde
      [ fresh () (q === Std.pair !!true __) (rez === !!1)
      ; fresh temp (q =/= Std.pair !!true temp) (rez === !!2)
      ]
  ;;

  let%expect_test "Without disequalities at all" =
    let xxx q rez =
      conde
        [ fresh () (q === Std.pair !!true __) (rez === !!1)
        ; fresh (l r) (q === Std.pair l r) (rez === !!2)
        ]
    in
    test ~explicit:false xxx;
    [%expect
      {|
      fun q ->
        fresh (scru rhs) (q === (pair scru rhs)) (rel scru rhs)
          (fresh (l r) (scru === (Std.pair l r))
             (if explicit then (bool_dom l) &&& (bool_dom r) else success)), all answers {
      q=((true, _.17), 1);
      q=((_.18, _.19), 2);
      } |}];
    test ~explicit:true xxx;
    [%expect
      {|
      fun q ->
        fresh (scru rhs) (q === (pair scru rhs)) (rel scru rhs)
          (fresh (l r) (scru === (Std.pair l r))
             (if explicit then (bool_dom l) &&& (bool_dom r) else success)), all answers {
      q=((true, false), 1);
      q=((true, true), 1);
      q=((false, false), 2);
      q=((true, false), 2);
      q=((false, true), 2);
      q=((true, true), 2);
      } |}]
  ;;

  let%expect_test " " =
    print_endline "Naive with diseq constraints (6 answers instead of 4): ";
    test ~explicit:false naive_rel;
    [%expect
      {|
      Naive with diseq constraints (6 answers instead of 4):
      fun q ->
        fresh (scru rhs) (q === (pair scru rhs)) (rel scru rhs)
          (fresh (l r) (scru === (Std.pair l r))
             (if explicit then (bool_dom l) &&& (bool_dom r) else success)), all answers {
      q=((true, _.16), 1);
      q=((_.17, _.18 [=/= _.13]), 2);
      q=((_.17 [=/= true], _.18), 2);
      } |}]
  ;;

  let smart_rel q rez =
    conde
      [ fresh () (q === Std.pair !!true __) (rez === !!1)
      ; fresh () (q =/= Std.pair !!true __) (rez === !!2)
      ]
  ;;

  let%expect_test " " =
    print_endline "With wildcards (explicit domain):";
    test smart_rel;
    [%expect
      {|
      With wildcards (explicit domain):
      fun q ->
        fresh (scru rhs) (q === (pair scru rhs)) (rel scru rhs)
          (fresh (l r) (scru === (Std.pair l r))
             (if explicit then (bool_dom l) &&& (bool_dom r) else success)), all answers {
      q=((true, false), 1);
      q=((false, false), 2);
      q=((true, true), 1);
      q=((false, true), 2);
      } |}];
    print_endline "With wildcards (no domain):";
    test ~explicit:false smart_rel;
    [%expect
      {|
      With wildcards (no domain):
      fun q ->
        fresh (scru rhs) (q === (pair scru rhs)) (rel scru rhs)
          (fresh (l r) (scru === (Std.pair l r))
             (if explicit then (bool_dom l) &&& (bool_dom r) else success)), all answers {
      q=((true, _.15), 1);
      q=((_.16 [=/= true], _.17), 2);
      } |}]
  ;;
end

module _ = struct
  let source =
    {|
    match ... with
    | _,f,t -> 1
    | f,t,_ -> 2
    | _,_,f -> 3
    | _,_,t -> 4
  |}
  ;;

  let%expect_test " " =
    Printf.printf "*******\n\nLonger example for Luc's Maranget paper\n";
    Printf.printf "Pseudecode:\n%s\n" source;
    [%expect
      {|
      *******

      Longer example for Luc's Maranget paper
      Pseudecode:

          match ... with
          | _,f,t -> 1
          | f,t,_ -> 2
          | _,_,f -> 3
          | _,_,t -> 4 |}]
  ;;

  let run_m eta =
    run_r
      (Pair.reify (Triple.reify OCanren.reify OCanren.reify OCanren.reify) OCanren.reify)
      ([%show:
         ( (GT.bool logic, GT.bool logic, GT.bool logic) Std.Triple.logic
           , GT.int logic )
           Std.Pair.logic]
         ())
      eta
  ;;

  let test ?(explicit = true) rel =
    [%tester
      run_m (-1) (fun q ->
        fresh
          (scru rhs l m r)
          (q === pair scru rhs)
          (rel scru rhs)
          (scru === Std.triple l m r)
          (if explicit then bool_dom l &&& bool_dom m &&& bool_dom r else success))]
  ;;

  let smart_rel q rez =
    let _T = !!true in
    let _F = !!false in
    let w = Std.triple in
    conde
      [ fresh () (rez === !!1) (q === w __ _F _T)
      ; fresh () (rez === !!2) (q === w _F _T __) (q =/= w __ _F _T)
      ; fresh () (rez === !!3) (q === w __ __ _F) (q =/= w __ _F _T) (q =/= w _F _T __)
      ; fresh
          ()
          (rez === !!4)
          (q =/= w __ _F _T)
          (q =/= w _F _T __)
          (q =/= w __ __ _F)
          (q === w __ __ _T)
      ]
  ;;

  let%expect_test " " =
    print_endline "With wildcards (explicit domain): ";
    test smart_rel;
    [%expect
      {|
      With wildcards (explicit domain):
      fun q ->
        fresh (scru rhs l m r) (q === (pair scru rhs)) (rel scru rhs)
          (scru === (Std.triple l m r))
          (if explicit
           then ((bool_dom l) &&& (bool_dom m)) &&& (bool_dom r)
           else success), all answers {
      q=((false, false, true), 1);
      q=((true, false, true), 1);
      q=((false, true, false), 2);
      q=((false, true, true), 2);
      q=((true, true, true), 4);
      q=((false, false, false), 3);
      q=((true, false, false), 3);
      q=((true, true, false), 3);
      } |}];
    print_endline "With wildcards (no explicit domain): ";
    test ~explicit:false smart_rel;
    [%expect
      {|
      With wildcards (no explicit domain):
      fun q ->
        fresh (scru rhs l m r) (q === (pair scru rhs)) (rel scru rhs)
          (scru === (Std.triple l m r))
          (if explicit
           then ((bool_dom l) &&& (bool_dom m)) &&& (bool_dom r)
           else success), all answers {
      q=((_.13, false, true), 1);
      q=((false, true, _.15), 2);
      q=((_.13, _.14 [=/= true], false), 3);
      q=((_.13, _.14 [=/= false; =/= true], true), 4);
      q=((_.13 [=/= false], _.14, false), 3);
      q=((_.13 [=/= false], _.14 [=/= false], true), 4);
      } |}]
  ;;

  let naive_rel q rez =
    let _T = !!true in
    let _F = !!false in
    let w = Std.triple in
    conde
      [ fresh () (rez === !!1) (q === w __ _F _T)
      ; fresh x (rez === !!2) (q === w _F _T __) (q =/= w x _F _T)
      ; fresh (x z) (rez === !!3) (q === w __ __ _F) (q =/= w x _F _T) (q =/= w _F _T z)
      ; fresh
          (x y z x2 y2)
          (rez === !!4)
          (q =/= w x _F _T)
          (q =/= w _F _T z)
          (q =/= w x y _F)
          (q === w x2 y2 _T)
      ]
  ;;

  let%expect_test " " =
    test ~explicit:false naive_rel;
    [%expect
      {|
      fun q ->
        fresh (scru rhs l m r) (q === (pair scru rhs)) (rel scru rhs)
          (scru === (Std.triple l m r))
          (if explicit
           then ((bool_dom l) &&& (bool_dom m)) &&& (bool_dom r)
           else success), all answers {
      q=((_.13, false, true), 1);
      q=((false, true, _.15), 2);
      q=((_.13, _.14, false), 3);
      q=((_.13, _.14 [=/= false], true), 4);
      q=((_.13 [=/= _.21], _.14, true), 4);
      } |}]
  ;;

  let%expect_test " " =
    [%tester
      run_int (-1) (fun rhs -> fresh s (s === Std.triple !!true __ __) (smart_rel s rhs))];
    [%expect
      {|
      fun rhs -> fresh s (s === (Std.triple (!! true) __ __)) (smart_rel s rhs), all answers {
      q=1;
      q=3;
      q=4;
      } |}]
  ;;

  let hack q rez =
    let _T = !!true in
    let _F = !!false in
    let w = Std.triple in
    conde
      [ failure
        (* ; fresh () (rez === !!1) (q === w __ _F _T) *)
        (* ; fresh () (rez === !!2) (q === w _F _T __) (q =/= w __ _F _T) *)
        (* ; fresh () (rez === !!3) (q === w __ __ _F) (q =/= w __ _F _T) (q =/= w _F _T __) *)
      ; fresh
          ()
          (rez === !!4)
          (* (q === w __ __ __) *)
          (q =/= w __ _F _T)
          (q =/= w _F _T __)
          (q =/= w __ __ _F)
          (debug_var !!1 OCanren.reify (fun _ ->
             (* OCanren.set_diseq_logging true; *)
             success))
          (q === w __ __ _T)
      ]
  ;;

  let%expect_test " " =
    [%tester
      run_m (-1) (fun q ->
        fresh
          (scru rhs (* l m r*))
          (q === pair scru rhs)
          (* (scru === Std.triple l m r) *)
          (hack scru rhs)
          (* (bool_dom l) *)
          (* (bool_dom m) *)
          (* (bool_dom r) *)
          (* trace_diseq_constraints *)
          success)];
    [%expect
      {|
      fun q -> fresh (scru rhs) (q === (pair scru rhs)) (hack scru rhs) success, all answers {
      q=((_.13, _.14 [=/= false; =/= true], true), 4);
      q=((_.13 [=/= false], _.14 [=/= false], true), 4);
      } |}]
  ;;
end
