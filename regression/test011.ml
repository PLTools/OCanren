1open GT
open MiniKanren
open Tester

@type t = N | A of t logic with show

let show_t         = show(logic) (show t)
let show_int       = show(logic) (show int)
let show_list      = show(logic) (show list show_int)
let show_list_list = show(logic) (show list show_list)
let show_llist     = show(logic) (show llist (show int))

let int_reifier dc x =
  match reify dc x with
  | [] -> ""
  | cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_int x) (show_int c)) cs

let list_reifier dc t = 
  let h = Hashtbl.create 10 in
  let check_var i = Hashtbl.mem h i in
  let add_var   i = Hashtbl.add h i true in
  match t with
  | (Var i) as x ->
      if check_var i 
      then ""
      else begin
	add_var i;
        match reify dc x with
	| [] -> ""
	| cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_list x) (show_list c)) cs
      end
  | Value l -> 
      List.fold_left (fun acc -> function (Var i) as x when not (check_var i) -> add_var i; acc ^ int_reifier dc x | _ -> acc) "" l

let _ = 
  run show_t         empty_reifier (-1) q (fun q st -> REPR((fresh(x) (x =/= !(A x)))                                                                   st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                          st), ["q", q]);
  run show_list_list empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !1)(![x; y] === q)) st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !0))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(z === !0)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                          st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(z === !1)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(z === !1)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2))                                             st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(y === !1))                                             st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1))                                   st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(![x; y] === q))                                        st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(![x; y] === q))                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !9)(![x; y] === q))                    st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5)(d === !6))                          st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1)(![x; y] === q))                    st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === z))                                   st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === !5)(![x; z] === q))                   st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((!3 =/= !4)                                                                                st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((!3 =/= !3)                                                                                st), ["q", q]);
  run show_int       int_reifier   (-1) q (fun q st -> REPR (((!5 =/= q) &&& (!6 =/= q) &&& (q === !5))                                                 st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5))                                    st), ["q", q]);
  run show_int       int_reifier   (-1) q (fun q st -> REPR ((fresh (a)(!3 === a)(a =/= !4))                                                            st), ["q", q]);
  run show_int       int_reifier   (-1) q (fun q st -> REPR (((!4 =/= q) &&& (!3 =/= q))                                                                st), ["q", q]);
  run show_int       int_reifier   (-1) q (fun q st -> REPR (((q =/= !5) &&& (q =/= !5))                                                                st), ["q", q]);
  run show_int       int_reifier   (-1) q (fun q st -> REPR ((let foo x = fresh (a)(x =/= a) in fresh(a)(foo a))                                        st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; y] === q)(x =/= y))                                                      st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (a d)(![a; d] === q)(q =/= ![!5; !6]))                                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !3))                                    st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; y] === q)(y =/= x))                                                      st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; y] === q)(x =/= y)(y =/= x))                                             st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; y] === q)(x =/= y)(x =/= y))                                             st), ["q", q]);
  run show_int       int_reifier   (-1) q (fun q st -> REPR (((q =/= !5) &&& (!5 =/= q))                                                                st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; y] === q)(![x; y] =/= ![!5; !6])(x =/= !5))                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; y] === q)(x =/= !5)(![x; y] =/= ![!5; !6]))                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(x =/= !5)(![x; y] =/= ![!5; !6])(![x; y] === q))                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(!5 =/= x)(![x; y] =/= ![!5; !6])(![x; y] === q))                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(!5 =/= x)(![y; x] =/= ![!6; !5])(![x; y] === q))                              st), ["q", q]);
  run show_list      list_reifier  (-1) q (fun x st -> REPR ((fresh (y z)(x =/= ![y; !2])(x === ![z; !2]))                                              st), ["x", x]);

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
   run show_int int_reifier (-1) q (fun q st -> REPR (distincto (!2 % (!3 %< q)) st), ["q", q]);

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
   run show_llist empty_reifier (-1) q (fun q st -> REPR (rembero !1 (!1 % (!2 % (!1 %< !3))) q          st), ["q", q]);
   run show_llist empty_reifier (-1) q (fun q st -> REPR (rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)) st), ["q", q]);

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
   run show_llist empty_reifier (-1) q (fun q st -> REPR (rembero !1 (!1 % (!2 % (!1 %< !3))) q          st), ["q", q]);
   run show_llist empty_reifier (-1) q (fun q st -> REPR (rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)) st), ["q", q])
