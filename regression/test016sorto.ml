open GT
open MiniKanren
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

let () = Nat.(
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> leo (inj_nat 1) (inj_nat 2) q ));
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> leo (inj_nat 2) (inj_nat 1) q ));
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> gto (inj_nat 1) (inj_nat 2) q ));
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> gto (inj_nat 2) (inj_nat 1) q ));

    run_exn show_nat  (-1)  qr qrh (REPR (fun q r -> minmaxo (inj_nat 1) (inj_nat 2)  q r ));
    ()
  )
