open GT
open MiniKanren
open Tester

@type 'a gt = N | A of 'a with show;;
type rt = rt gt                        (* autoreified *)
type ft = (ft gt, rt) fancy (* fancified *)
type lt = lt gt logic                  (* reified *)

let show_rt x =
  let rec helper : rt -> string = function
  | N -> "N"
  | A x -> "A (" ^ (helper x) ^ ")"
  in
  helper x
(* module N = struct
  type t1 = ft
  type t2 = rt
end

let _f x : int =
  let module H = FMapALike(struct type t1 = ft type t2 = rt end) in
  H.wrap (inj@@lift (A x)) *)

module N = struct
  type 'a t = 'a gt
  type r = rt
end
module NF = FMapALike2(N)
(* let _f x : int =
  let in
  H.wrap (inj@@lift (A x)) *)

let a x : ft = NF.wrap @@ inj@@lift (A x)
let n :   ft = NF.wrap @@ inj@@lift N


(* let show_t         = show(logic) (show t)
let show_int       = show(logic) (show int)
let show_list      = show(logic) (show list show_int)
let show_list_list = show(logic) (show list show_list)
let show_llist     = show(List.logic) (show(logic) (show int)) *)

let _ =
  run_exn show_rt         (-1) q (REPR (fun q -> (fresh(x) (x =/= (a x)))                                                                 )) qh;
  (* run show_int       (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                         )) qh;
  run show_list_list (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !1)(![x; y] === q)))) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !0))               )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y z)(z === !0)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))               )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y z)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                         )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y z)(z === !1)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))               )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y z)(z === !1)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))               )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2))                                            )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(y === !1))                                            )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1))                                  )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(![x; y] === q))                                       )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(![x; y] === q))                             )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !9)(![x; y] === q))                   )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5)(d === !6))                         )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1)(![x; y] === q))                   )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === z))                                  )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === !5)(![x; z] === q))                  )) qh;
  run show_list      (-1) q (REPR (fun q -> (!3 =/= !4)                                                                               )) qh;
  run show_list      (-1) q (REPR (fun q -> (!3 =/= !3)                                                                               )) qh;
  run show_int       (-1) q (REPR (fun q -> ((!5 =/= q) &&& (!6 =/= q) &&& (q === !5))                                                )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5))                                   )) qh;
  run show_int       (-1) q (REPR (fun q -> (fresh (a)(!3 === a)(a =/= !4))                                                           )) qh;
  run show_int       (-1) q (REPR (fun q -> ((!4 =/= q) &&& (!3 =/= q))                                                               )) qh;
  run show_int       (-1) q (REPR (fun q -> ((q =/= !5) &&& (q =/= !5))                                                               )) qh;
  run show_int       (-1) q (REPR (fun q -> (let foo x = fresh (a)(x =/= a) in fresh(a)(foo a))                                       )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y))                                                     )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6]))                                             )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !3))                                   )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(y =/= x))                                                     )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y)(y =/= x))                                            )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y)(x =/= y))                                            )) qh;
  run show_int       (-1) q (REPR (fun q -> ((q =/= !5) &&& (!5 =/= q))                                                               )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(![x; y] =/= ![!5; !6])(x =/= !5))                             )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= !5)(![x; y] =/= ![!5; !6]))                             )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(x =/= !5)(![x; y] =/= ![!5; !6])(![x; y] === q))                             )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(!5 =/= x)(![x; y] =/= ![!5; !6])(![x; y] === q))                             )) qh;
  run show_list      (-1) q (REPR (fun q -> (fresh (x y)(!5 =/= x)(![y; x] =/= ![!6; !5])(![x; y] === q))                             )) qh;
  run show_list      (-1) q (REPR (fun x -> (fresh (y z)(x =/= ![y; !2])(x === ![z; !2]))                                             )) (fun xs -> ["x", xs]); *)
  ()
(*
  let rec distincto l =
    conde [
      l === !Nil;
      (fresh (a) (l === !< a));
      (fresh (a ad dd) (
         (l === a % (ad % dd)) &&&
         (a =/= ad) &&&
         (distincto (a % dd)) &&&
         (distincto (ad % dd))
      ))
    ]
   in
   run show_int (-1) q (REPR (fun q -> distincto (!2 % (!3 %< q)))) qh;

   let rec rembero x ls out =
     conde [
       (ls === !Nil) &&& (out === !Nil);
       fresh (a d res) (
         (ls === a % d) &&&
         (rembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (out === a % res)
          ])
       )
     ]
   in
   run show_llist (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 % (!1 %< !3))) q         )) qh;
   run show_llist (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)))) qh;

   let rec rembero x ls out =
     conde [
       (ls === !Nil) &&& (out === !Nil);
       fresh (a d res) (
         (ls === a % d) &&&
         (rembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (a =/= x) &&& (out === a % res)
          ])
       )
     ]
   in
   run show_llist (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 % (!1 %< !3))) q         )) qh;
   run show_llist (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)))) qh *)
