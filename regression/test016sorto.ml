open GT
open MiniKanren
open MiniKanren.Std
open Tester

let show_nat_list = GT.(show LList.ground @@ show LNat.ground)
let show_nat      = GT.(show LNat.ground)

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max = LNat.(
    conde
      [ (min === a) &&& (max === b) &&& (a <= b)
      ; (max === a) &&& (min === b) &&& (a >  b)
      ]
  )

let () = 
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> LNat.leo (nat 1) (nat 2) q));
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> LNat.leo (nat 2) (nat 1) q));
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> LNat.gto (nat 1) (nat 2) q));
    run_exn GT.(show bool)  (-1)   q  qh (REPR (fun q   -> LNat.gto (nat 2) (nat 1) q));

    run_exn show_nat  (-1)  qr qrh (REPR (fun q r -> minmaxo (nat 1) (nat 2)  q r ));
    ()
  
