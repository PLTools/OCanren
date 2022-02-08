(* reimplementing Nat module *)
open OCanren
open Tester
open Printf
open GT

(* module Peano = struct

    module T =
      struct
        @type 'a t = O | S of 'a with show, gmap

        let fmap f x = gmap(t) f x
     end

  include T
  include Fmap (T)

  type ground = ground T.t
  type lnat = lnat t logic

  let o () = inj @@ distrib O
  let s x  = inj @@ distrib (S x)

  type rt = rt t              (* normal type *)
  type lt = lt t logic        (* reified     *)
  type ft = (rt, lt) injected (* injected    *)

  let rec show_ln n = show(logic) (show(t) show_ln) n
  let rec show_rn n = show(t) show_rn n

end
 *)

open OCanren.Std
let runN n = run_r Std.Nat.reify (GT.show Std.Nat.logic) n

let o      = Nat.zero
let s prev = Nat.succ prev

let rec addo x y z =
  conde [
    (x === o) &&& (z === y);
    Fresh.two (fun x' z' ->
      (x === s x') &&&
      (z === s z') &&&
      (addo x' y z')
    )
  ]

let rec mulo x y z =
  conde [
    (x === o) &&& (z === o);
    Fresh.two (fun x' z' ->
      (x === s x') &&&
      (addo y z' z) &&&
      (mulo x' y z')
    )
  ]

let () =
  runN  1    q   qh (REPR (fun q   -> addo o (s o) q                   ));
  runN  1    q   qh (REPR (fun q   -> addo (s o) (s o) q               ));
  runN  2    q   qh (REPR (fun q   -> addo o (s o) q                   ));
  runN  2    q   qh (REPR (fun q   -> addo (s o) (s o) q               ));
  runN  1    q   qh (REPR (fun q   -> addo q (s o) (s o)               ));
  runN  1    q   qh (REPR (fun q   -> addo (s o) q (s o)               ));
  runN  2    q   qh (REPR (fun q   -> addo q (s o) (s o)               ));
  runN  2    q   qh (REPR (fun q   -> addo (s o) q (s o)               ));
  runN (-1) qr  qrh (REPR (fun q r -> addo q r (s (s (s (s o))))       ));

  runN  1    q   qh (REPR (fun q   -> mulo o (s o) q                   ));
  runN  1    q   qh (REPR (fun q   -> mulo (s (s o)) (s (s o)) q       ));
  runN  2    q   qh (REPR (fun q   -> mulo o (s o) q                   ));
  runN  1    q   qh (REPR (fun q   -> mulo q (s (s o)) (s (s o))       ));
  runN  1    q   qh (REPR (fun q   -> mulo q (s (s o)) (s (s (s o)))   ));
  runN  2    q   qh (REPR (fun q   -> mulo q (s (s o)) (s (s o))       ));
  runN  2    q   qh (REPR (fun q   -> mulo q (s (s o)) (s (s (s o)))   ));

  runN  1    q   qh (REPR (fun q   -> mulo (s (s o)) q (s (s o))      ));
  runN  1    q   qh (REPR (fun q   -> mulo (s (s o)) q (s (s (s o)))  ));
  runN  2    q   qh (REPR (fun q   -> mulo (s (s o)) q (s (s o))      ));
  runN  2    q   qh (REPR (fun q   -> mulo (s (s o)) q (s (s (s o)))  ));

  runN  1   qr  qrh (REPR (fun q r -> mulo q (s o) r                  ));
  runN 10   qr  qrh (REPR (fun q r -> mulo q (s o) r                  ));

  runN  1   qr  qrh (REPR (fun q r -> mulo (s o) q r                  ));
  runN 10   qr  qrh (REPR (fun q r -> mulo (s o) q r                  ));

  runN  1   (succ q) (*qr*)  qrh (REPR (fun q r -> mulo q r (s o)                  ));

  runN  1    q   qh (REPR (fun q   -> mulo (s o) (s o) q              ));
  runN  1   qr  qrh (REPR (fun q r -> mulo q r (s (s (s (s o))))      ));
  runN  3   qr  qrh (REPR (fun q r -> mulo q r (s (s (s (s o))))      ))

let () =
  runN   1   qr  qrh (REPR (fun q r   -> mulo q r o                   ));
  runN   3  qrs qrsh (REPR (fun p q r -> mulo p q r                   ));
  runN  10  qrs qrsh (REPR (fun q r s -> mulo q r s                   ))
