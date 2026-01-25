(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2016-2026
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(* In this demo we benhmark default reverso in a lucky direction
  against debug_var reverso, which works in any direction.
  For longer lists, performance difference could be neglected
*)

open OCanren

let rec appendo xs ys zs =
  let open OCanren.Std in
  conde
    [
      xs === Std.nil () &&& (ys === zs);
      fresh (h tl tmp) (xs === h % tl) (zs === h % tmp) (appendo tl ys tmp);
    ]

let rec reverso xs rez =
  let open OCanren.Std in
  conde
    [
      xs === nil () &&& (xs === rez);
      (* Good order for forward execution *)
      fresh (h tl tmp) (xs === h % tl) (reverso tl tmp) (appendo tmp !<h rez);
    ]

let rec reverso_hacky xs rez =
  let open OCanren.Std in
  conde
    [
      xs === nil () &&& (xs === rez);
      debug_var xs [%reify: GT.int OCanren.Std.List.ground] (function
        | [ Var _ ] ->
            fresh (h tl tmp)
              (xs === h % tl)
              (appendo tmp !<h rez) (reverso_hacky tl tmp)
        | _ ->
            fresh (h tl tmp)
              (xs === h % tl)
              (reverso_hacky tl tmp) (appendo tmp !<h rez));
    ]

let () =
  let open OCanren.Std in
  let open Tester in
  [%tester
    run_r [%reify: GT.int OCanren.Std.List.ground]
      ([%show: GT.int OCanren.logic OCanren.Std.List.logic] ())
      (-1)
      (fun q -> reverso (OCanren.Std.list OCanren.inj [ 1; 2; 3 ]) q)]

let () =
  let open OCanren.Std in
  let open Tester in
  [%tester
    run_r [%reify: GT.int OCanren.Std.List.ground]
      ([%show: GT.int OCanren.logic OCanren.Std.List.logic] ())
      (-1)
      (fun q -> reverso_hacky q (OCanren.Std.list OCanren.inj [ 1; 2; 3 ]))]

let make_list n : _ ilogic =
  assert (n > 0);
  let rec helper acc n =
    if n <= 0 then acc else helper (Std.List.cons !!1 acc) (n - 1)
  in
  helper (Std.nil ()) n

let () =
  let xs700 = make_list 500 in
  let test rel () =
    let s =
      run q
        (fun v -> rel xs700 v)
        (fun rr -> rr#reify [%reify: GT.int OCanren.Std.List.ground])
    in
    let answers = OCanren.Stream.take s in
    assert (List.length answers = 1)
  in
  let open Benchmark in
  let res =
    throughputN ~style:Nil ~repeat:2 2
      [ ("default", test reverso, ()); ("debug_var", test reverso_hacky, ()) ]
  in
  tabulate res
