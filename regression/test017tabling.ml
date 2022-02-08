open OCanren
open Tester

let patho_norec arco po x y = conde [
  arco x y;
  Fresh.one (fun z ->
    (arco x z) &&& (po z y)
  );
]

let rec patho arco = patho_norec arco (fun x y -> delay @@ fun () -> patho arco x y)

let patho_tabled arco = Tabling.(tabledrec two) (patho_norec arco)

let show s = s
let showl = GT.show(logic) (show)

(* This relation defines a graph with cycle *)
let arco1 x y = conde [
  (x === !!"a") &&& (y === !!"b");
  (x === !!"b") &&& (y === !!"a");
  (x === !!"b") &&& (y === !!"d");
]

let run_path n = run_r OCanren.reify showl n
let _ =
  (* Check that query to tabled goal terminates and results are stored to the cache *)
  let patho_tabled = patho_tabled arco1 in
  run_path 10 q qh (REPR(fun q -> patho arco1 !!"a" q));
  run_path 10 q qh (REPR(fun q -> patho_tabled !!"a" q));
  run_path 10 q qh (REPR(fun q -> patho_tabled !!"a" q))

let _ =
  (* Check that external disequalities are checked *)
  let patho_tabled = patho_tabled arco1 in
  run_path 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho arco1 !!"a" q)));
  run_path 10 q qh (REPR(fun q -> (patho_tabled !!"a" q)));
  run_path 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho_tabled !!"a" q)))

let _ =
  (* Check that external disequalities do not affect the cache *)
  let patho_tabled = patho_tabled arco1 in
  run_path 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho arco1 !!"a" q)));
  run_path 10 q qh (REPR(fun q -> (q =/= !!"b") &&& (patho_tabled !!"a" q)));
  run_path 10 q qh (REPR(fun q -> (patho_tabled !!"a" q)))

(* This relation defines a graph with cycle where one of nodes is not ground *)
let arco2 x y =
  fresh (z)
    (conde [
      (x === !!"a") &&& (y === z);
      (x === z) &&& (y === !!"a");
    ])

let runS n = run_r OCanren.reify showl n
let _ =
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
