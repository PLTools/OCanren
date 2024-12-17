open OCanren
open Tester

let rel list1 =
  let open OCanren.Std in
  fresh
    (list2 hd1 tl1 hd2 tl2)
    (list1 =/= list2)
    (list1 === hd1 % tl1)
    (list2 === hd2 % tl2)
    (hd2 === !!1)
    (tl2 === nil ())
    (hd1 === !!1)
    (tl1 === nil ())
;;

(* let () = [%tester run_r [%show GT.int GT.list] (Std.List.reify reify) 1 (fun q -> rel q)] *)
let () = run_r (Std.List.reify reify) ([%show: GT.int logic Std.List.logic] ()) 1 q qh (REPR rel)
