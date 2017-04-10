open Printf
open MiniKanren
open Tester

let (!) = inj_int
let (!!) = List.of_list

let show_int_list         = GT.(show List.ground @@ show int)
let show_int_list_list    = GT.(show List.ground @@ show List.ground @@ show int)
let show_intl_llist       = GT.(show List.logic @@ show logic @@ show int)
let show_intl_llist_llist = GT.(show List.logic @@ show List.logic @@ show logic @@ show int)

let runInt n = runR ManualReifiers.int_reifier GT.(show int) GT.(show logic @@ show int) n
let runIList n = runR (List.reify ManualReifiers.int_reifier) show_int_list show_intl_llist n
let runIListList n = runR (List.reify @@ List.reify ManualReifiers.int_reifier) show_int_list_list show_intl_llist_llist n

let _ =
  runInt       (-1) q qh (REPR (fun q -> (q =/= !1)       ));
  runIList     (-1) q qh (REPR (fun q -> (fresh (x y z)(x =/= y)(x === !![!0; z; !1])(y === !![!0; !1; !1]))                   ));
  runIList     (-1) q qh (REPR (fun q -> (fresh (x y z)(x =/= y)(x === !![!0; z; !1])(y === !![!0; !1; !1])(z === !0))         ));
  runIList     (-1) q qh (REPR (fun q -> (fresh (x y z)(z === !0)(x =/= y)(x === !![!0; z; !1])(y === !![!0; !1; !1]))         ));
  runIList     (-1) q qh (REPR (fun q -> (fresh (x y z)(z === !1)(x =/= y)(x === !![!0; z; !1])(y === !![!0; !1; !1]))         ));
  ()

let _ =
  runIList       (-1) q qh (REPR (fun q -> (fresh (x y z)(x === !![!0; z; !1])(y === !![!0; !1; !1])(x =/= y))                   ));
  runIList       (-1) q qh (REPR (fun q -> (fresh (x y z)(z === !1)(x === !![!0; z; !1])(y === !![!0; !1; !1])(x =/= y))         ));
  ()
let _ =
  runIList      (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; !1] =/= !![!2; y])(x === !2))                                       ));
  runIList      (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; !1] =/= !![!2; y])(y === !1))                                       ));
  runIList      (-1) q qh (REPR (fun q -> (fresh (x y)
                                                    (!![x; !1] =/= !![!2; y])
                                                    (!![x; y] === q))
                                                  ));

  runIList      (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; !1] =/= !![!2; y])(x === !2)(!![x; y] === q))                     ));
  runIList      (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; !1] =/= !![!2; y])(x === !2)(y === !9)(!![x; y] === q))           ));
  runIList      (-1) q qh (REPR (fun q -> (fresh (a d)(!![a; d] === q)(q =/= !![!5; !6])(a === !5)(d === !6))                  ));
  runIList      (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; !1] =/= !![!2; y])(x === !2)(y === !1)(!![x; y] === q))           ));
  runIList      (-1) q qh (REPR (fun q -> (fresh (a x z)(a =/= !![x; !1])(a === !![z; !1])(x === z))                           ));
  ()

let _ =
  runIList      (-1) q qh (REPR (fun q -> (fresh (a x z)(a =/= !![x; !1])(a === !![z; !1])(x === !5)(!![x; z] === q))         ));
  ()

let _ =
  runInt                (-1) q qh (REPR (fun q -> (!3 =/= !4) ));
  runInt                (-1) q qh (REPR (fun q -> (!3 =/= !3)                                              ));
  run_exn GT.(show int) (-1) q qh (REPR (fun q -> ((!5 =/= q) &&& (!6 =/= q) &&& (q === !5))               ));
  runIList              (-1) q qh (REPR (fun q -> (fresh (a d)(!![a; d] === q)(q =/= !![!5; !6])(a === !5))  ));
  runInt                (-1) q qh (REPR (fun q -> (fresh (a)(!3 === a)(a =/= !4))                          ));
  runInt                (-1) q qh (REPR (fun q -> ((!4 =/= q) &&& (!3 =/= q))                              ));
  runInt                (-1) q qh (REPR (fun q -> ((q =/= !5) &&& (q =/= !5))                              ));

  runInt                (-1) q qh (REPR (fun q -> (let foo x = fresh (a)(x =/= a) in fresh(a)(foo a))      ));
  runIList            (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; y] === q)(x =/= y))                     ));
  runIList            (-1) q qh (REPR (fun q -> Fresh.two (fun a d -> ?& [ !![a; d] === q; q =/= !![!5; !6] ]) ));
  runIList            (-1) q qh (REPR (fun q -> Fresh.two (fun a d -> ?& [ !![a; d] === q; q =/= !![!5; !6];
                                                                          (a === !3) ])                    ));
  runIList            (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; y] === q)(y =/= x))                    ));
  runIList            (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; y] === q)(x =/= y)(y =/= x))           ));
  runIList            (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; y] === q)(x =/= y)(x =/= y))           ));
  runInt              (-1) q qh (REPR (fun q -> ((q =/= !5) &&& (!5 =/= q))                  ));

  runIList           (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; y] === q)(!![x; y] =/= !![!5; !6])(x =/= !5))                 ));
  runIList           (-1) q qh (REPR (fun q -> (fresh (x y)(!![x; y] === q)(x =/= !5)(!![x; y] =/= !![!5; !6]))                 ));
  runIList           (-1) q qh (REPR (fun q -> (fresh (x y)(x =/= !5)(!![x; y] =/= !![!5; !6])(!![x; y] === q))                 ));
  runIList           (-1) q qh (REPR (fun q -> (fresh (x y)(!5 =/= x)(!![x; y] =/= !![!5; !6])(!![x; y] === q))                 ));
  runIList           (-1) q qh (REPR (fun q -> (fresh (x y)(!5 =/= x)(!![y; x] =/= !![!6; !5])(!![x; y] === q))                 ));

  let xsh xs = ["xs", xs] in
  runIList (-1) q xsh (REPR (fun x -> (fresh (y z)(x =/= !![y; !2])(x === !![z; !2]))));

  runIListList (-1) q qh (REPR (fun q -> (fresh (x y z)(q =/= x)(x =/= !![y])(y =/= !![z]) )           ));
  ()


(* next test goes into stackoverflow in ICFP 2016 version *)
let () =
  let runL n = runR (List.reify @@ List.reify ManualReifiers.int_reifier) (fun _ -> assert false)
                (GT.show List.logic (GT.show List.logic @@ GT.show logic string_of_int ))
                n in
  let wrap x y = Fresh.one (fun tempH -> x =/= tempH % y) in
  runL              (-1) q qh (REPR (fun q -> (fresh (x y z)(x === q)(wrap x y)(wrap y z) (wrap z x) ) ));
  ()

let () =
  let rec distincto l =
    conde [
      l === nil ();
      (fresh (a) (l === !< a));
      (fresh (a ad dd) (
         (l === a % (ad % dd)) &&&
         (a =/= ad) &&&
         (distincto (a % dd)) &&&
         (distincto (ad % dd))
      ))
    ]
  in
  runInt (-1) q qh (REPR (fun q -> distincto (!2 % (!3 %< q)) ));

  let rec remembero x ls out =
     conde [
       (ls === nil ()) &&& (out === nil ());
       fresh (a d res) (
         (ls === a % d) &&&
         (remembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (out === a % res)
          ])
       )
     ]
  in
  run_exn show_int_list (-1) q qh (REPR (fun q -> remembero !1 (!1 % (!2 % (!1 %< !3))) q          ));
  runInt                (-1) q qh (REPR (fun q -> remembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)) ));

   let rec rembero x ls out =
     conde [
       (ls === nil ()) &&& (out === nil ());
       fresh (a d res) (
         (ls === a % d) &&&
         (rembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (a =/= x) &&& (out === a % res)
          ])
       )
     ]
   in
   run_exn show_int_list (-1) q qh (REPR (fun q -> rembero !1 (!1 % (!2 % (!1 %< !3))) q          ));
   runInt                (-1) q qh (REPR (fun q -> rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)) ))
