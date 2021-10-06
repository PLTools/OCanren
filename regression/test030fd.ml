open OCanren
open Tester
(*
let rel1 x =
  fresh (temp)
    (FD.domain x [1;2])
    (FD.neq x !!1)
    (FD.neq x !!2)*)


let runL eta = runR OCanren.reify GT.(show int) GT.(show logic @@ show int) eta

let rel3 dom a b c d =
  fresh (_temp)
    (FD.domain a dom)
    (FD.domain b dom)
    (FD.domain c dom)
    (FD.domain d dom)
    (FD.lt a b)
    (FD.lt b c)
    (FD.lt c d)

let _freeVars =
  runL   1  q     qh (REPR (fun x -> (FD.domain x [1;2])  ));
  flush stdout;
  runL   1  q     qh (REPR (fun x -> (FD.domain x [1;2]) &&&  (FD.neq x !!1)  ));
  runL   1  q     qh (REPR (fun x -> (FD.domain x [1;2]) &&&  (FD.neq x !!1) &&& (FD.neq x !!2) ));
  runL   1  q     qh (REPR (fun x -> (FD.domain x [1;2]) &&&  (FD.neq x !!1) ));
  runL   1  qrst  qrsth (REPR (rel3 [1;2;3]));
  runL   1  qrst  qrsth (REPR (rel3 [1;2;3;4]));
  ()
