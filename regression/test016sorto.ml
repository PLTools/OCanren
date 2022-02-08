open GT
open OCanren
open OCanren.Std
open Tester

let show_nat_list = GT.(show List.ground @@ show Nat.ground)
let show_nat      = GT.(show Nat.ground)

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max = Nat.(
    conde
      [ (min === a) &&& (max === b) &&& (a <= b)
      ; (max === a) &&& (min === b) &&& (a >  b)
      ]
  )

let run n = run_r prj_exn GT.(show bool) n
let run_nat n = run_r Nat.prj_exn GT.(show Nat.ground) n
let () =
    run  (-1)   q  qh (REPR (fun q   -> Nat.leo (nat 1) (nat 2) q));
    run  (-1)   q  qh (REPR (fun q   -> Nat.leo (nat 2) (nat 1) q));
    run  (-1)   q  qh (REPR (fun q   -> Nat.gto (nat 1) (nat 2) q));
    run  (-1)   q  qh (REPR (fun q   -> Nat.gto (nat 2) (nat 1) q));

    run_nat (-1)  qr qrh (REPR (fun q r -> minmaxo (nat 1) (nat 2)  q r ));
    ()
