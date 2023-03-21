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
  ocanrun (q, r : ^Nat.t) {addo q r 2} -> (show(Nat.logic q), show(Nat.logic) r)

(*                                            
  run qr
    (fun q r -> addo q r (Nat.s (Nat.s Nat.o)))
    (fun q r -> (show(Nat.logic) (q#reify Nat.reify)), (show(Nat.logic) (r#reify Nat.reify)))
 *)

let _ =
  L.iter (fun q -> printf "q=%s\n" q) @@
  Stream.take ~n:(-1) @@
  run q
    (fun q -> addo q (Nat.s Nat.o) Nat.o)
    (fun q -> (show(Nat.logic) (q#reify Nat.reify)))
