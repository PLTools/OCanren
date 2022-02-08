(* SPDX-License-Identifier: LGPL-2.1-or-later *)
open OCanren

let rec mylen1 xs rez =
  let open OCanren.Std in
  conde
  [ fresh (h tl p) (xs === h%tl) (rez === Nat.succ p) (mylen1 tl p)
  ; fresh () (xs === nil()) (rez === Nat.zero)
  ]

let rec mylen2 xs rez =
  let open OCanren.Std in
  conde
  [ fresh (h tl p) (xs === h%tl) (mylen2 tl p) (rez === Nat.succ p)
  ; fresh () (xs === nil()) (rez === Nat.zero)
  ]



let make_list n : _ ilogic =
  assert (n>0);
  let rec helper acc n =
    if n<= 0 then acc
    else helper (Std.List.cons !!1 acc) (n-1)
  in
  helper (Std.nil ()) n

let () =
  let xs700 = make_list 700 in
  let test rel () =
    let s = run q (rel xs700) (fun rr -> rr#reify Std.Nat.prj_exn) in
    assert (not (Stream.is_empty s));
    let nat = Stream.hd s in
    assert (Std.Nat.to_int nat = 700);
  in
  let open Benchmark  in
  let res = throughputN ~style:Nil ~repeat:2 2
      [
        ("test1", test mylen1, ());
        ("test2", test mylen2, ());
      ]
  in
  tabulate res




(*
(* https://github.com/kajigor/mk-transformers-bench/blob/master/experiments/length/len0.ml *)
let len0 y1 y2 =
  let rec len xs l =
    (((xs === (List.nil ())) &&& (l === Nat.zero)) |||
    (fresh (m t h)
      (((xs === (h % t)) &&&
       ((l === (Nat.succ m)) &&&
       (len t m))))))
  in  (len y1 y2)

(* https://github.com/kajigor/mk-transformers-bench/blob/master/experiments/length/len2.ml *)
let len2 y1 y2 =
  let rec len xs l =
    ( (fresh (m t h)
       (((xs === (h % t)) &&&
         ((len t m) &&&
         (l === (Nat.succ m)))))) |||
      ((xs === (List.nil ())) &&& (l === Nat.zero))
    )
  in  (len y1 y2) *)
