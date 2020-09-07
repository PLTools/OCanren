open OCanren
open OCanren.Std
open Tester
open Printf
open GT

let show_int       = show(int)
let show_bool       = show(bool)
let show_int_opt   = show(option) (show(int))
let show_intl      = show(logic)  (show(int))
let show_booll      = show(logic)  (show(bool))
let show_intl_optl = show(logic)  (show(option) (show(logic) (show(int))))


let runInt eta = runR OCanren.reify (GT.show(GT.int)) show_intl eta
let runBool eta = runR OCanren.reify (GT.show(GT.bool)) (show(logic) @@ show bool) eta



let f_ _ =
  runInt 1 q qh (REPR(fun q -> wc (fun __ -> q === __) ));
  runInt 1 q qh (REPR(fun q -> wc (fun __ -> __ === q) ));
  runInt 1 q qh (REPR(fun q -> wc (fun __ -> wc (fun __ -> __ === __)) ));

  runInt 1 q qh (REPR(fun q -> wc (fun __ -> __ =/= __) ));
  runInt 1 q qh (REPR(fun q -> wc (fun __ -> __ =/= q) ));
  runInt 1 q qh (REPR(fun q -> wc (fun __ -> wc (fun __ -> __ =/= __)) ));
  ()



let is_empty l r = wc (fun __1 -> wc (fun __2 ->
  conde [
    (l === nil ())  &&& (r === !!true);
    (l === __1 % __2) &&& (r === !!false)
  ]))


let f_ _ =
  runBool (-1) q qh (REPR(fun q -> is_empty (Std.nil()) q));
  runBool (-1) q qh (REPR(fun q -> is_empty (!< !!42) q));
  runBool (-1) q qh (REPR(fun q -> fresh (two) (is_empty two q) ));
  ()


let fst_is_true p r = wc (fun __ ->
  conde [
  (p === pair !!true __) &&& (r === !!true);
  (p =/= pair !!true __) &&& (r === !!false)
])

let pair_has_true p r = wc (fun __ -> conde [
  (p === pair !!true __) &&& (r === !!1);
  (p =/= pair !!true __) &&& (p === pair __ !!true) &&& (r === !!2);
  (p =/= pair !!true __) &&& (p =/= pair __ !!true) &&& (r === !!3)
])
let pair_has_true p r = wc (fun __ -> conde [
  (p === pair !!true __) &&& (r === !!1);
  fresh (w)
    (p =/= pair !!true __)
    (p === pair w !!true)
    (r === !!2);
  (p =/= pair !!true __) &&& (p =/= pair __ !!true) &&& (r === !!3)
])

let runBool eta = runR OCanren.reify (GT.show(GT.bool)) (show(logic) @@ show bool) eta
let runPair eta = runR (Pair.reify OCanren.reify OCanren.reify) (GT.show(Pair.ground) show_int show_int)
  (show Pair.logic show_intl show_intl) eta
let runPairB eta = runR (Pair.reify OCanren.reify OCanren.reify) (GT.show(Pair.ground) show_bool show_bool)
  (show Pair.logic show_booll show_booll) eta

let  _ =
  runPair (-1) q qh (REPR(fun q ->
    fresh (a b)
    (q =/= pair a !!1)
    (q === pair !!1 b)
  ));
  (* q=(1, _.12); *)
  runPair (-1) q qh (REPR(fun q ->
    fresh (a b)
    (q =/= pair a !!1)
    (q === pair !!1 !!1)
  ));
  (* q=(1, 1); *)
  runPair (-1) q qh (REPR(fun q ->
    fresh (a b)
    (q =/= pair a !!1)
    (q === pair !!1 !!1)
    (a === !!1)
  ));
  (* no answers *)
  ()
let _f =

  runInt (-1) q qh (REPR(fun q -> pair_has_true (pair !!true !!true) q) );
  runInt (-1) q qh (REPR(fun q -> fresh (r) (pair_has_true (pair r !!true) q)) );
  runInt (-1) q qh (REPR(fun q -> fresh (r t) (pair_has_true (pair r t) q)) );
  runInt (-1) q qh (REPR(fun q -> fresh (r) (pair_has_true r q) )) ;

  (* runPair (-1) q qh (REPR(fun q -> wc (fun __ ->
    (q =/= pair !!1 __) &&& (q === pair __ !!1)
  )) );

  runPairB (-1) q qh (REPR(fun q -> wc (fun __ ->
    pair __ !!true =/= pair !!true __
  )));

  runPairB (-1) q qh (REPR(fun q ->
    wc (fun __ ->
      fresh (a b)
        (q === pair a b)
        (q =/= pair !!true __)
        (q === pair __ !!true)
    )));

  runPairB (-1) q qh (REPR(fun q ->
    wc (fun __ ->
      fresh (a b)
        (q =/= pair !!true __)
        (q === pair __ !!true)
        (q === pair a b)
    ))); *)

  (* runInt (-1) q qh (REPR(fun q -> fresh (r) (wc (fun __ ->
    (r =/= pair !!true __) &&& (r === pair __ !!true)
  )) ));  *)
(*
  runInt (-1) q qh (REPR(fun q -> wc (fun __ ->
    pair  !!true __ =/= (pair __ !!true)
  ) ));
  runInt (-1) q qh (REPR(fun q -> wc (fun __ ->
    pair  __ __ =/= (pair !!true !!true)
  ) ));

  runInt (-1) q qh (REPR(fun q -> wc (fun __ ->
    pair  !!true __ === (pair __ !!true)
  ) )); *)

    (*
  runInt (-1) q qh (REPR(fun q -> fresh (r) (wc (fun __ ->
    (r === pair __ !!true) &&& (r =/= pair !!true __)
  )) )) ; *)

  (* runInt (-1) q qh (REPR(fun q ->  wc (fun __ -> (pair !!1 q =/= pair __ !!2)) ));
  runInt (-1) q qh (REPR(fun q ->  wc (fun __ -> (pair !!1 q =/= pair __ !!2) &&& (q === !!2)) ));
  *)
  ()

let _f _ =
  runInt (-1) q qh (REPR(fun q -> (wc (fun __ ->
      fresh (r)
        (pair __ !!2 =/= r)
        (r === pair !!1 q)
  ))));


  (*
    I    (1, _.10) =/= (__, 2)  ok
    II   _.10 === 2  failure

    *********
    I    (__, 2) =/= _.11 ok
    II   _.11 === (1, _.10)       и мы ожидаем один  констрейт 2 =/= _.11


  *)
  runInt (-1) q qh (REPR(fun q -> (wc (fun __ ->
    fresh (r)
      (pair !!2 __  =/= r)
      (r === pair q !!1)
  ))));
  (*
      I    (2, __) =/= _.11 ok
      II   _.11 === (_.10, 1)       и мы ожидаем один  констрейт 2 =/= _.11

  *)

  runInt (-1) q qh (REPR(fun q -> (wc (fun __ ->
    fresh (r)
      (pair !!2 __  =/= r)
      (r === pair !!2 !!1)
  ))));
  (*
      I    (2, __) =/= _.11 ok
      II   _.11 === (2, 1)       и мы ожидаем failure

  *)
  ()
