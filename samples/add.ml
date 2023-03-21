module L = List
         
open GT
open Printf
open OCanren
open OCanren.Std

let addo x y z =
  let open Nat in
  ocanren {
    x == o & y == z |
    fresh x', z' in
      x == S x' & z == S z' & addo x' y z'
  }

let _ =
  L.iter (fun (q, r) -> printf "q=%s, r=%s\n" q r) @@
  Stream.take ~n:(-1) @@
  ocanrun (q, r : ^Nat.nat) {addo q r 2} -> (show(Nat.logic) q, show(Nat.logic) r)

let _ =
  L.iter (fun q -> printf "q=%s\n" q) @@
  Stream.take ~n:(-1) @@
  ocanrun (q : ^Nat.nat) {addo q 1 0} -> (show(Nat.logic) q)
