open GT
open MiniKanren
open Tester

@type nat = O | S of nat logic with show

let rec addo x y z =
  conde [
    (x === !O) &&& (z === y);
    fresh (x' z')
       (x === !(S x'))
       (z === !(S z'))
       (addo x' y z')
  ]

let rec mulo x y z =
  conde [
    (x === !O) &&& (z === !O);
    fresh (x' z') 
      (x === !(S x')) 
      (addo y z' z)
      (mulo x' y z')
  ]

let show_nat = show logic (show nat)

let _ = 
  run show_nat  1    q  (REPR (fun q   -> addo !O !(S !O) q                    )) qh;
  run show_nat  1    q  (REPR (fun q   -> addo !(S !O) !(S !O) q               )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo !O !(S !O) q                    )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo !(S !O) !(S !O) q               )) qh;
  run show_nat  1    q  (REPR (fun q   -> addo q !(S !O) !(S !O)               )) qh;
  run show_nat  1    q  (REPR (fun q   -> addo !(S !O) q !(S !O)               )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo q !(S !O) !(S !O)               )) qh;
  run show_nat  2    q  (REPR (fun q   -> addo !(S !O) q !(S !O)               )) qh;
  run show_nat (-1) qr  (REPR (fun q r -> addo q r !(S !(S !(S !(S !O))))      )) qrh;

  run show_nat  1    q  (REPR (fun q   -> mulo !O !(S !O) q                    )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo !(S !(S !O)) !(S !(S !O)) q     )) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo !O !(S !O) q                    )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo q !(S !(S !O)) !(S !(S !O))     )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo q !(S !(S !O)) !(S !(S !(S !O))))) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo q !(S !(S !O)) !(S !(S !O))     )) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo q !(S !(S !O)) !(S !(S !(S !O))))) qh;

  run show_nat  1    q  (REPR (fun q   -> mulo !(S !(S !O)) q !(S !(S !O))     )) qh;
  run show_nat  1    q  (REPR (fun q   -> mulo !(S !(S !O)) q !(S !(S !(S !O))))) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo !(S !(S !O)) q !(S !(S !O)) )    ) qh;
  run show_nat  2    q  (REPR (fun q   -> mulo !(S !(S !O)) q !(S !(S !(S !O))))) qh;
  
  run show_nat  1   qr  (REPR (fun q r -> mulo q !(S !O) r                     )) qrh;
  run show_nat 10   qr  (REPR (fun q r -> mulo q !(S !O) r                     )) qrh;

  run show_nat  1   qr  (REPR (fun q r -> mulo !(S !O) q r                     )) qrh;
  run show_nat 10   qr  (REPR (fun q r -> mulo !(S !O) q r                     )) qrh;

  run show_nat  1   qr  (REPR (fun q r -> mulo q r !O                          )) qrh;
  run show_nat  1   qr  (REPR (fun q r -> mulo q r !(S !O)                     )) qrh;
  
  run show_nat  1    q  (REPR (fun q   -> mulo !(S !O) !(S !O) q               )) qh;
  run show_nat  1   qr  (REPR (fun q r -> mulo q r !(S !(S !(S !(S !O))))      )) qrh;
  run show_nat  3   qr  (REPR (fun q r -> mulo q r !(S !(S !(S !(S !O))))      )) qrh;

  run show_nat  3  qrs  (REPR (fun q r s -> mulo q r s                         )) qrsh;
  run show_nat 10  qrs  (REPR (fun q r s -> mulo q r s                         )) qrsh

