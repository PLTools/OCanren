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
       (defer (addo x' y z'))
  ]

let rec mulo x y z =
  conde [
    (x === !O) &&& (z === !O);
    fresh (x' z') 
      (x === !(S x')) 
      (addo y z' z)
      (defer (mulo x' y z'))
  ]

let show_nat = show logic (show nat)

let _ = 
  run show_nat   1    q  (fun q     st -> REPR (addo !O !(S !O) q                     st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (addo !(S !O) !(S !O) q                st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (addo !O !(S !O) q                     st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (addo !(S !O) !(S !O) q                st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (addo q !(S !O) !(S !O)                st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (addo !(S !O) q !(S !O)                st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (addo q !(S !O) !(S !O)                st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (addo !(S !O) q !(S !O)                st), ["q", q]);
  run show_nat (-1)  qp  (fun q p   st -> REPR (addo q p !(S !(S !(S !(S !O))))       st), ["q", q; "p", p]);

  run show_nat   1    q  (fun q     st -> REPR (mulo !O !(S !O) q                     st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (mulo !(S !(S !O)) !(S !(S !O)) q      st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (mulo !O !(S !O) q                     st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !O))      st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !(S !O))) st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !O))      st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !(S !O))) st), ["q", q]);

  run show_nat   1    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !O))      st), ["q", q]);
  run show_nat   1    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !(S !O))) st), ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !O)) st)     , ["q", q]);
  run show_nat   2    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !(S !O))) st), ["q", q]);
  
  run show_nat   1   qp  (fun q p   st -> REPR (mulo q !(S !O) p                      st), ["q", q; "p", p]);
  run show_nat  10   qp  (fun q p   st -> REPR (mulo q !(S !O) p                      st), ["q", q; "p", p]);

  run show_nat   1   qp  (fun q p   st -> REPR (mulo !(S !O) q p                      st), ["q", q; "p", p]);
  run show_nat  10   qp  (fun q p   st -> REPR (mulo !(S !O) q p                      st), ["q", q; "p", p]);

  run show_nat   1   qp  (fun q p   st -> REPR (mulo q p !O                           st), ["q", q; "p", p]);
  run show_nat   1   qp  (fun q p   st -> REPR (mulo q p !(S !O)                      st), ["q", q; "p", p]);
  
  run show_nat   1    q  (fun q     st -> REPR (mulo !(S !O) !(S !O) q                st), ["q", q]);
  run show_nat   1   qp  (fun q p   st -> REPR (mulo q p !(S !(S !(S !(S !O))))       st), ["q", q; "p", p]);
  run show_nat   3   qp  (fun q p   st -> REPR (mulo q p !(S !(S !(S !(S !O))))       st), ["q", q; "p", p]);

  run show_nat   3  qpr  (fun q p r st -> REPR (mulo q p r                            st), ["q", q; "p", p; "r", r]);
  run show_nat  10  qpr  (fun q p r st -> REPR (mulo q p r                            st), ["q", q; "p", p; "r", r])

