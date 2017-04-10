open MiniKanren
open Tester

let rec patho arco x y = conde [
  arco x y;
  Fresh.one (fun z ->
    (arco x z) &&& (patho arco z y)
  )
]

let patho_tabled arco =
  let patho_norec po x y = conde [
    arco x y;
    Fresh.one (fun z ->
      (arco x z) &&& (po z y)
    );
  ] in
  let patho = ref (fun x y -> assert false) in
  let patho_rec x y = patho_norec !patho x y in
  let patho_tabled = Tabling.(tabled two) patho_rec in
  patho := patho_tabled;
  patho_tabled

let show s = s
let showl = GT.show(logic) (show)

let runS n = runR (MiniKanren.reify) show showl n

(* This relation defines a graph with cycle *)
let arco1 x y = conde [
  (x === !!"a") &&& (y === !!"b");
  (x === !!"b") &&& (y === !!"a");
  (x === !!"b") &&& (y === !!"d");
]

let _ =
  (* Check that query to tabled goal terminates and results are stored to the cache *)
  let patho_tabled = patho_tabled arco1 in
  run_exn show 10 q qh (REPR(fun q -> patho arco1 !!"a" q));
  run_exn show 10 q qh (REPR(fun q -> patho_tabled !!"a" q));
  run_exn show 10 q qh (REPR(fun q -> patho_tabled !!"a" q))

let _ =
  (* Check that external disequalities are checked *)
  let patho_tabled = patho_tabled arco1 in
  run_exn show 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho arco1 !!"a" q)));
  run_exn show 10 q qh (REPR(fun q -> (patho_tabled !!"a" q)));
  run_exn show 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho_tabled !!"a" q)))


let _ =
  (* Check that external disequalities do not affect the cache *)
  let patho_tabled = patho_tabled arco1 in
  run_exn show 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho arco1 !!"a" q)));
  run_exn show 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho_tabled !!"a" q)));
  run_exn show 10 q qh (REPR(fun q -> (patho_tabled !!"a" q)))

(* This relation defines a graph with cycle where one of nodes is not ground *)
let arco2 x y =
  fresh (z)
    (conde [
      (x === !!"a") &&& (y === z);
      (x === z) &&& (y === !!"a");
    ])

(* let _ = *)
  (* Check that tabling works with non-ground terms *)
  let patho_tabled = patho_tabled arco2 in
  runS 10 q qh (REPR(fun q -> patho arco2 !!"a" q));
  runS 10 q qh (REPR(fun q -> patho_tabled !!"a" q));
  runS 10 q qh (REPR(fun q -> patho_tabled !!"a" q))


(* This relation defines a graph with cycle where one of nodes is not ground but constrained *)
let arco3 x y =
    (conde [
      (x === !!"a") &&& (y =/= x) &&& (y =/= !!"b");
      (x =/= !!"b") &&& (y =/= x) &&& (y === !!"a");
    ])

let _ =
  (* Check that internal constraints are stored to the cache *)
  let patho_tabled = patho_tabled arco3 in
  runS 10 q qh (REPR(fun q -> patho arco3 !!"a" q));
  runS 10 q qh (REPR(fun q -> patho_tabled !!"a" q));
  runS 10 q qh (REPR(fun q -> patho_tabled !!"a" q))
