open MiniKanren
open Tester
open Printf

(* Can't do this without specifying Show class for fancy type in MiniKanren.ml *)
(* @type nat = O | S of ((nat,nat logic) MiniKanren.fancy) with show;; *)

type 'self gnat =
  | O
  | S of 'self
  | MetaVar of int

type nat = nat gnat
type natf = (natf gnat, nat) fancy
type natl = natl gnat logic

let show_nat n =
  let b = Buffer.create 10 in
  let rec helper = function
  | O -> bprintf b "O"
  | S m ->
    bprintf b "S(";
    helper m;
    bprintf b ")"
  | MetaVar m -> bprintf b "_.%d" m
  in
  helper n;
  Buffer.contents b

let show_nat_gen foo n =
  let b = Buffer.create 10 in
  let rec helper = function
  | O -> bprintf b "O"
  | S m ->
    bprintf b "S(";
    foo b helper m;
    bprintf b ")"
  | MetaVar m -> bprintf b "_.%d" m
  in
  foo b helper n;
  Buffer.contents b

let show_natf n = show_nat_gen bprintf_fancy n
let show_natl n = show_nat_gen bprintf_logic n

let nat_reifier (cond: var_checker) (x : natf) : natl =
  let rec helper x =
    if cond#isVar x then refine_fancy3 x cond helper
    else match coerce_fancy x with
    | MetaVar n -> failwith "This constructor should not happen"
    | O -> Value O
    | S n -> Value (S (helper n))
  in
  helper x

module TypFamilies = FMapALike0(struct
  type t = natf gnat
  type r = nat  gnat
end)

(* TODO: do we need extra thunk here? *)
let o      : natf = TypFamilies.wrap @@ inj@@lift O
let s prev : natf = TypFamilies.wrap @@ inj@@lift @@ S prev

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

let runN c = run_exn show_nat c

let _ =
  runN  1    q  (REPR (fun q   -> addo o (s o) q                    )) qh;
  runN  1    q  (REPR (fun q   -> addo (s o) (s o) q               )) qh;
  runN  2    q  (REPR (fun q   -> addo o (s o) q                    )) qh;
  runN  2    q  (REPR (fun q   -> addo (s o) (s o) q               )) qh;
  runN  1    q  (REPR (fun q   -> addo q (s o) (s o)               )) qh;
  runN  1    q  (REPR (fun q   -> addo (s o) q (s o)               )) qh;
  runN  2    q  (REPR (fun q   -> addo q (s o) (s o)               )) qh;
  runN  2    q  (REPR (fun q   -> addo (s o) q (s o)               )) qh;
  runN (-1) qr  (REPR (fun q r -> addo q r (s (s (s (s o))))      )) qrh;

  runN  1    q  (REPR (fun q   -> mulo o (s o) q                    )) qh;
  runN  1    q  (REPR (fun q   -> mulo (s (s o)) (s (s o)) q     )) qh;
  runN  2    q  (REPR (fun q   -> mulo o (s o) q                    )) qh;
  runN  1    q  (REPR (fun q   -> mulo q (s (s o)) (s (s o))     )) qh;
  runN  1    q  (REPR (fun q   -> mulo q (s (s o)) (s (s (s o))))) qh;
  runN  2    q  (REPR (fun q   -> mulo q (s (s o)) (s (s o))     )) qh;
  runN  2    q  (REPR (fun q   -> mulo q (s (s o)) (s (s (s o))))) qh;

  runN  1    q  (REPR (fun q   -> mulo (s (s o)) q (s (s o))     )) qh;
  runN  1    q  (REPR (fun q   -> mulo (s (s o)) q (s (s (s o))))) qh;
  runN  2    q  (REPR (fun q   -> mulo (s (s o)) q (s (s o)) )    ) qh;
  runN  2    q  (REPR (fun q   -> mulo (s (s o)) q (s (s (s o))))) qh;

  runN  1   qr  (REPR (fun q r -> mulo q (s o) r                     )) qrh;
  runN 10   qr  (REPR (fun q r -> mulo q (s o) r                     )) qrh;

  runN  1   qr  (REPR (fun q r -> mulo (s o) q r                     )) qrh;
  runN 10   qr  (REPR (fun q r -> mulo (s o) q r                     )) qrh;

  runN  1   qr  (REPR (fun q r -> mulo q r (s o)                     )) qrh;

  runN  1    q  (REPR (fun q   -> mulo (s o) (s o) q               )) qh;
  runN  1   qr  (REPR (fun q r -> mulo q r (s (s (s (s o))))      )) qrh;
  runN  3   qr  (REPR (fun q r -> mulo q r (s (s (s (s o))))      )) qrh;
  ()

let () =
  runR nat_reifier show_nat show_natl  1   qr  (REPR (fun q r   -> mulo q r o                         )) qrh;
  runR nat_reifier show_nat show_natl  3  qrs  (REPR (fun p q r -> mulo p q r                         )) qrsh;
  runR nat_reifier show_nat show_natl 10  qrs  (REPR (fun q r s -> mulo q r s                         )) qrsh;
  ()
