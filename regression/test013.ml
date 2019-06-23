open Printf
open MiniKanren
open MiniKanren.Std
open Tester

let show_nat        = GT.show(LNat.ground)
let show_bool       = GT.show(LBool.ground)

let show_nat_llist  = GT.show(LList.ground) (GT.show(LNat.ground))
let show_bool_llist = GT.show(LList.ground) (GT.show(LBool.ground))
let show_option_nat = GT.(show option  (show LNat.ground))

let (?$) = nat
let nats = nat_list
let bools = list (!!)

let sumo = LList.foldro LNat.addo ?$0

let () =
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.noto LBool.truo  q                       ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.noto LBool.falso q                       ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.noto q          LBool.truo               ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.oro   LBool.falso LBool.falso q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.oro   LBool.falso LBool.truo  q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.oro   LBool.truo  LBool.falso q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.oro   LBool.truo  LBool.truo  q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.ando  LBool.falso LBool.falso q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.ando  LBool.falso LBool.truo  q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.ando  LBool.truo  LBool.falso q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> LBool.ando  LBool.truo  LBool.truo  q            ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.addo ?$0 ?$1 q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.addo ?$1 q   ?$3                           ));
  run_exn show_nat         3   qr qrh (REPR (fun q r   -> LNat.addo q   r   q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.mulo ?$1 ?$2 q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.mulo ?$3 q   ?$6                           ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.mulo ?$3 q   ?$6                           ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.mulo ?$3 ?$0 q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> LNat.mulo q   ?$5 ?$0                           ));
  run_exn show_nat         3    q  qh (REPR (fun q     -> LNat.mulo q   ?$0 ?$0                           ))

let () =
  run_exn show_nat         1    q  qh (REPR (fun q     -> sumo (nats []) q                               ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> sumo (nats [3;1;2]) q                          ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> sumo (?$0 % (?$1 % (q %< ?$3))) ?$6            ));
  ()

let () =
  run_exn show_nat         1    q   qh (REPR (fun q     -> LList.lengtho (nats [1;2;3;4]) q                    ));
  run_exn show_nat         1    q   qh (REPR (fun q     -> LList.lengtho (list (!!) [(); (); ()]) q    ));
  run_exn show_nat         1    q   qh (REPR (fun q     -> LList.lengtho (bools [false; true]) q               ));
  run_exn show_nat         1    q   qh (REPR (fun q     -> LList.lengtho (nats [4;3;2;1;0]) q                  ));
  run_exn show_nat_llist   1    q   qh (REPR (fun q     -> LList.lengtho q ?$0                                 ));

  run_exn show_bool        1    q   qh (REPR (fun q     -> LList.anyo (bools [false; false; true]) q               ));
  run_exn show_bool        1    q   qh (REPR (fun q     -> LList.anyo (bools [false; false]) q                     ));

  run_exn show_bool        1    q   qh (REPR (fun q     -> LList.allo (bools [true; false; true]) q                ));
  run_exn show_bool        1    q   qh (REPR (fun q     -> LList.allo (LBool.truo % (q %< LBool.truo)) LBool.truo     ));
  run_exn show_bool      (-1) qrs qrsh (REPR (fun q r s -> LList.allo (LBool.truo % (q %< r)) s                     ))

let _ =
  run_exn show_nat_llist    1    q  qh (REPR (fun q     -> LList.mapo (LNat.addo ?$1) (nats [0;1;2]) q              ));
  run_exn show_nat_llist    1    q  qh (REPR (fun q     -> LList.mapo (LNat.addo ?$2) q (nats [4;3;2])              ));
  run_exn show_nat          1    q  qh (REPR (fun q     -> LList.mapo (LNat.addo q) (nats [1;2;3]) (nats [4;5;6])   ));
  run_exn show_nat          1    q  qh (REPR (fun q     -> LList.mapo (LNat.mulo q) (nats [1;2;3]) (nats [2;4;6])   ));
  run_exn show_nat          1   qr qrh (REPR (fun q r   -> LList.mapo (LNat.mulo q) (nats [1;2]) (?$2 %< r)         ));
  run_exn show_nat_llist    1    q  qh (REPR (fun q     -> LList.mapo (===) (nats [1;2;3]) q                       ));
  run_exn show_nat          1    q  qh (REPR (fun q     -> LList.mapo (===) (nats [1;2;3]) (?$1 % (?$2 %< q))      ));
  run_exn show_bool_llist   1    q  qh (REPR (fun q     -> LList.mapo LBool.noto (bools [true;false;true;]) q       ));
  run_exn show_bool_llist   1    q  qh (REPR (fun q     -> LList.mapo LBool.noto (bools []) q                       ));

  run_exn show_nat_llist  (-1)   q  qh (REPR (fun q     -> LList.filtero (eqo ?$2) (nats [0;1;2;3]) q              ));
  run_exn show_option_nat   1    q  qh (REPR (fun q     -> LList.lookupo (eqo ?$1) (nats [0;2;1;3]) q              ))

let show_nat_list   = GT.(show LList.ground @@ show LNat.ground)
let show_natl_listl = GT.(show LList.logic  @@ show LNat.logic)

let runN n = runR LNat.reify show_nat (GT.show(LNat.logic)) n
let runL n = runR (LList.reify LNat.reify) show_nat_list show_natl_listl n

let _freeVars =
  runN         3   qr qrh (REPR (fun q r   -> LNat.mulo q   r   q             ));
  runL      (-1)    q  qh (REPR (fun q     -> LList.lengtho q ?$3             ))
