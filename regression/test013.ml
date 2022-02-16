open Printf
open OCanren
open OCanren.Std
open Tester

let show_nat        = GT.show(Nat.ground)
let show_bool       = GT.show(Bool.ground)

let show_nat_List  = GT.show(List.ground) (GT.show(Nat.ground))
let show_bool_List = GT.show(List.ground) (GT.show(Bool.ground))
let show_option_nat = GT.(show option (show Nat.ground))

let (?$) = nat
let nats = nat_list
let bools = list (!!)

let sumo = List.foldro Nat.addo ?$0

let run_bool n = run_r OCanren.prj_exn show_bool n
let run_nat n = run_r Nat.prj_exn (GT.show Nat.ground) n
let run_nat_list n = run_r (List.prj_exn Nat.prj_exn) (GT.show List.ground @@ GT.show Nat.ground) n
let run_bool_list n = run_r (List.prj_exn Bool.prj_exn) (GT.show List.ground @@ GT.show Bool.ground) n
let run_option_nat n = run_r (Option.prj_exn Nat.prj_exn) (GT.show Option.ground @@ GT.show Nat.ground) n

let () =
  run_bool        1    q  qh (REPR (fun q     -> Bool.noto Bool.truo  q                       ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.noto Bool.falso q                       ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.noto q          Bool.truo               ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.oro   Bool.falso Bool.falso q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.oro   Bool.falso Bool.truo  q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.oro   Bool.truo  Bool.falso q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.oro   Bool.truo  Bool.truo  q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.ando  Bool.falso Bool.falso q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.ando  Bool.falso Bool.truo  q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.ando  Bool.truo  Bool.falso q            ));
  run_bool        1    q  qh (REPR (fun q     -> Bool.ando  Bool.truo  Bool.truo  q            ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.addo ?$0 ?$1 q                             ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.addo ?$1 q   ?$3                           ));
  run_nat         3   qr qrh (REPR (fun q r   -> Nat.addo q   r   q                             ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$1 ?$2 q                             ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$3 q   ?$6                           ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$3 q   ?$6                           ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$3 ?$0 q                             ));
  run_nat         1    q  qh (REPR (fun q     -> Nat.mulo q   ?$5 ?$0                           ));
  run_nat         3    q  qh (REPR (fun q     -> Nat.mulo q   ?$0 ?$0                           ))

let () =
  run_nat         1    q  qh (REPR (fun q     -> sumo (nats []) q                               ));
  run_nat         1    q  qh (REPR (fun q     -> sumo (nats [3;1;2]) q                          ));
  run_nat         1    q  qh (REPR (fun q     -> sumo (?$0 % (?$1 % (q %< ?$3))) ?$6            ));
  ()

let () =
  run_nat         1    q   qh (REPR (fun q     -> List.lengtho (nats [1;2;3;4]) q                    ));
  run_nat         1    q   qh (REPR (fun q     -> List.lengtho (list (!!) [(); (); ()]) q    ));
  run_nat         1    q   qh (REPR (fun q     -> List.lengtho (bools [false; true]) q               ));
  run_nat         1    q   qh (REPR (fun q     -> List.lengtho (nats [4;3;2;1;0]) q                  ));
  run_nat_list    1    q   qh (REPR (fun q     -> List.lengtho q ?$0                                 ));

  run_bool        1    q   qh (REPR (fun q     -> List.anyo (bools [false; false; true]) q               ));
  run_bool        1    q   qh (REPR (fun q     -> List.anyo (bools [false; false]) q                     ));

  run_bool        1    q   qh (REPR (fun q     -> List.allo (bools [true; false; true]) q                ));
  run_bool        1    q   qh (REPR (fun q     -> List.allo (Bool.truo % (q %< Bool.truo)) Bool.truo     ));
  run_bool      (-1) qrs qrsh (REPR (fun q r s -> List.allo (Bool.truo % (q %< r)) s                     ))

let _ =
  run_nat_list    1    q  qh (REPR (fun q      -> List.mapo (Nat.addo ?$1) (nats [0;1;2]) q              ));
  run_nat_list    1    q  qh (REPR (fun q      -> List.mapo (Nat.addo ?$2) q (nats [4;3;2])              ));
  run_nat         1    q  qh (REPR (fun q      -> List.mapo (Nat.addo q) (nats [1;2;3]) (nats [4;5;6])   ));
  run_nat         1    q  qh (REPR (fun q      -> List.mapo (Nat.mulo q) (nats [1;2;3]) (nats [2;4;6])   ));
  run_nat         1   qr qrh (REPR (fun q r    -> List.mapo (Nat.mulo q) (nats [1;2]) (?$2 %< r)         ));
  run_nat_list    1    q  qh (REPR (fun q      -> List.mapo (===) (nats [1;2;3]) q                       ));
  run_nat         1    q  qh (REPR (fun q      -> List.mapo (===) (nats [1;2;3]) (?$1 % (?$2 %< q))      ));
  run_bool_list   1    q  qh (REPR (fun q      -> List.mapo Bool.noto (bools [true;false;true;]) q       ));
  run_bool_list   1    q  qh (REPR (fun q      -> List.mapo Bool.noto (bools []) q                       ));

  run_nat_list  (-1)   q  qh (REPR (fun q    -> List.filtero (eqo ?$2) (nats [0;1;2;3]) q              ));
  run_option_nat   1   q  qh (REPR (fun q    -> List.lookupo (eqo ?$1) (nats [0;2;1;3]) q              ))

let show_nat_list   = GT.(show List.ground @@ show Nat.ground)
let show_natl_listl = GT.(show List.logic  @@ show Nat.logic)

let runN n = run_r Nat.reify (GT.show(Nat.logic)) n
let runL n = run_r (List.reify Nat.reify) show_natl_listl n

let _freeVars =
  runN         3   qr qrh (REPR (fun q r   -> Nat.mulo q   r   q             ));
  runL      (-1)    q  qh (REPR (fun q     -> List.lengtho q ?$3             ))
