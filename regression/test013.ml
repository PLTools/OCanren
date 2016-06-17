open GT
open MiniKanren
open Tester

let show_nat        = show(Nat.logic)
let show_bool       = show(Bool.logic)
let show_nat_list   = show(List.logic) (show Nat.logic)
let show_bool_list  = show(List.logic) (show Bool.logic)
let show_option_nat = show(logic) (show(option) (show (Nat.logic)))
let show_int        = show(logic) (show(int))
let show_int_list   = show(List.logic) show_int

let (?$) = inj_nat
let nats = inj_nat_list

open Bool
open Nat
open List 

let sumo = foldro addo ?$0

let _ =
  run show_bool         1    q (REPR (fun q     -> noto' !true  q                                ))   qh;
  run show_bool         1    q (REPR (fun q     -> noto' !false q                                ))   qh;
  run show_bool         1    q (REPR (fun q     -> noto' q      !true                            ))   qh;
  run show_bool         1    q (REPR (fun q     -> oro  !false !false q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> oro  !false !true  q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> oro  !true  !false q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> oro  !true  !true  q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> ando !false !false q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> ando !false !true  q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> ando !true  !false q                          ))   qh;
  run show_bool         1    q (REPR (fun q     -> ando !true  !true  q                          ))   qh;
  run show_nat          1    q (REPR (fun q     -> addo ?$0 ?$1 q                                ))   qh;
  run show_nat          1    q (REPR (fun q     -> addo ?$1 q   ?$3                              ))   qh;
  run show_nat          3   qr (REPR (fun q r   -> addo q   r   q                                ))  qrh;
  run show_nat          1    q (REPR (fun q     -> mulo ?$1 ?$2 q                                ))   qh;
  run show_nat          1    q (REPR (fun q     -> mulo ?$3 q   ?$6                              ))   qh;
  run show_nat          1    q (REPR (fun q     -> mulo ?$3 ?$0 q                                ))   qh;
  run show_nat          3   qr (REPR (fun q r   -> mulo q   r   q                                ))  qrh;
  run show_nat          1    q (REPR (fun q     -> mulo q   ?$5 ?$0                              ))   qh;
  run show_nat          3    q (REPR (fun q     -> mulo q   ?$0 ?$0                              ))   qh;
  run show_nat          1    q (REPR (fun q     -> sumo (inj_list []) q                          ))   qh; 

  run show_nat          1    q (REPR (fun q     -> sumo (nats [3;1;2]) q                         ))   qh;  

  run show_nat          1    q (REPR (fun q     -> sumo (?$0 % (?$1 % (q %< ?$3))) ?$6           ))   qh; 
  

  run show_nat          1    q (REPR (fun q     -> lengtho (inj_list [1;2;3;4]) q                ))   qh; 
  run show_nat          1    q (REPR (fun q     -> lengtho (inj_list [(); (); ()]) q             ))   qh; 
  run show_nat          1    q (REPR (fun q     -> lengtho (inj_list [false; true]) q            ))   qh; 
  run show_nat          1    q (REPR (fun q     -> lengtho (nats [4;3;2;1;0]) q                  ))   qh; 
  run show_nat_list     1    q (REPR (fun q     -> lengtho q ?$3                                 ))   qh; 
  run show_nat_list     1    q (REPR (fun q     -> lengtho q ?$0                                 ))   qh; 
  run show_bool         1    q (REPR (fun q     -> anyo (inj_list [false;false;true]) q          ))   qh; 
  run show_bool         1    q (REPR (fun q     -> anyo (inj_list [false;false]) q               ))   qh; 
  run show_bool         1    q (REPR (fun q     -> allo (inj_list [true;false;true]) q           ))   qh; 
  run show_bool         1    q (REPR (fun q     -> allo (!true % (q %< !true)) !true             ))   qh; 
  run show_bool       (-1) qrs (REPR (fun q r s -> allo (!true % (q %< r)) s                     )) qrsh; 
  run show_nat_list     1    q (REPR (fun q     -> mapo (addo ?$1) (nats [0;1;2]) q              ))   qh;
  run show_nat_list     1    q (REPR (fun q     -> mapo (addo ?$2) q (nats [4;3;2])              ))   qh;
  run show_nat          1    q (REPR (fun q     -> mapo (addo q) (nats [1;2;3]) (nats [4;5;6])   ))   qh;
  run show_nat          1    q (REPR (fun q     -> mapo (mulo q) (nats [1;2;3]) (nats [2;4;6])   ))   qh;
  run show_nat          1   qr (REPR (fun q r   -> mapo (mulo q) (nats [1;2]) (?$2 %< r)         ))  qrh;
  run show_int_list     1    q (REPR (fun q     -> mapo (===) (inj_list [1;2;3]) q               ))   qh; 
  run show_int          1    q (REPR (fun q     -> mapo (===) (inj_list [1;2;3]) (!1 % (!2 %< q))))   qh; 
  run show_bool_list    1    q (REPR (fun q     -> mapo noto' (inj_list [true;false;true;]) q    ))   qh;
  run show_bool_list    1    q (REPR (fun q     -> mapo noto' (inj_list []) q                    ))   qh;
  run show_nat_list   (-1)   q (REPR (fun q     -> filtero (eqo ?$2) (nats [0;1;2;3]) q          ))   qh; 
  run show_option_nat   1    q (REPR (fun q     -> lookupo (eqo ?$1) (nats [0;2;1;3]) q          ))   qh

