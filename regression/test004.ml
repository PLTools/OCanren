open GT
open MiniKanren
open Tester
open Printf

(* Can't do this without specifying Show class for fancy type in MiniKanren.ml *)
(* @type nat = O | S of ((nat,nat logic) MiniKanren.fancy) with show;; *)

type nat =
  | O
  | S of (nat,nat logic) fancy
  | MetaVar of int

let show_nat n =
  let b = Buffer.create 10 in
  let rec helper = function
  | O -> bprintf b "O"
  | S m ->
    bprintf b "S(";
    bprintf_fancy b helper m;
    bprintf b ")"
  | MetaVar m -> bprintf b "_.%d" m
  in
  helper n;
  Buffer.contents b

let nat_reifier (cond: Obj.t -> bool) (x : nat) : nat =
  let rec helper x =
    if cond @@ Obj.repr x then MetaVar 0
    else match x with
    | MetaVar n -> MetaVar n
    | O -> O
    | S n -> S (Obj.magic @@ helper @@ Obj.magic n)
  in
  helper x

let o = inj@@lift O
let s prev = inj @@ lift @@ S prev

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

let _ =
  run show_nat  1    q  (REPR (fun q   -> addo o (s o) q                    )) qh;
  run show_nat  1    q  (REPR (fun q   -> addo (s o) (s o) q               )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo o (s o) q                    )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo (s o) (s o) q               )) qh;
  run show_nat  1    q  (REPR (fun q   -> addo q (s o) (s o)               )) qh;
  run show_nat  1    q  (REPR (fun q   -> addo (s o) q (s o)               )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo q (s o) (s o)               )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo (s o) q (s o)               )) qh;
  run show_nat (-1) qr  (REPR (fun q r -> addo q r (s (s (s (s o))))      )) qrh;

  (* run show_nat  1    q  (REPR (fun q   -> mulo o (s o) q                    )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo (s (s o)) (s (s o)) q     )) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo o (s o) q                    )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo q (s (s o)) (s (s o))     )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo q (s (s o)) (s (s (s o))))) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo q (s (s o)) (s (s o))     )) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo q (s (s o)) (s (s (s o))))) qh;

  run show_nat  1    q  (REPR (fun q   -> mulo (s (s o)) q (s (s o))     )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo (s (s o)) q (s !(s (s o))))) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo (s (s o)) q (s (s o)) )    ) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo (s (s o)) q (s !(s (s o))))) qh;

  run show_nat  1   qr  (REPR (fun q r -> mulo q (s o) r                     )) qrh;
  run show_nat 10   qr  (REPR (fun q r -> mulo q (s o) r                     )) qrh;

  run show_nat  1   qr  (REPR (fun q r -> mulo (s o) q r                     )) qrh;
  run show_nat 10   qr  (REPR (fun q r -> mulo (s o) q r                     )) qrh;

  run show_nat  1   qr  (REPR (fun q r -> mulo q r !o                          )) qrh;
  run show_nat  1   qr  (REPR (fun q r -> mulo q r (s o)                     )) qrh;

  run show_nat  1    q  (REPR (fun q   -> mulo (s o) (s o) q               )) qh;
  run show_nat  1   qr  (REPR (fun q r -> mulo q r (s (s (s (s o))))      )) qrh;
  run show_nat  3   qr  (REPR (fun q r -> mulo q r (s (s (s (s o))))      )) qrh; *)

  ()

let run = runR (fun isVar f ->
  try let ans = f () in
      ans
  with WithFreeVars(f,x) -> Obj.magic @@ nat_reifier isVar (Obj.magic x)
  )

let () =
  run show_nat  3  qrs  (REPR (fun q r s -> mulo q r s                         )) qrsh;
  run show_nat 10  qrs  (REPR (fun q r s -> mulo q r s                         )) qrsh;
  ()
