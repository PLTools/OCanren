(* This is a test suite for a lot of disequality constaints
 * mixed with plain OCaml lists
 *)
open Printf
open MiniKanren
open Tester

let (!) x = inj@@lift x

(* int logic of int fancy *)
let il_of_if cond g =
  let x = Obj.magic @@ g () in
  let rec foo : int -> int logic = fun x ->
    printf "il_of_if.foo '%s'\n%!" (generic_show x);
    if cond !!!x then refine_fancy !!!x !!!foo
    else Value x
  in
  foo x
let show_int = string_of_int
let show_if : (int,int)fancy -> _ = show_fancy @@ string_of_int
let show_il x =
  printf "show_il '%s'\n%!" (generic_show x);
  show_logic string_of_int x

let runN printer = runR il_of_if printer show_il
let _ =
  (* runN show_int         (-1) q (REPR (fun q -> (q =/= !1) )         ) qh; *)
  ()
;;
@type 'a gt = N | A of 'a with show;;
type rt = rt gt                        (* autoreified *)
type ft = (ft gt, rt) fancy (* fancified *)
type lt = (lt gt) logic                  (* reified *)

let show_rt x =
  let rec helper : rt -> string = function
  | N -> "N"
  | A x -> "A (" ^ (helper x) ^ ")"
  in
  helper x

let show_ln (x: lt) : string =
  printf "gshow of Value 1 '%s'\n%!" (generic_show @@ Value 1);
  printf "show_ln '%s'\n%!" (generic_show x);
  let rec helper: lt  -> string = function
  | Value N -> "N"
  | Value (A x) -> "A (" ^ (show_logic helper2 x) ^ ")"
  | Var _ as v -> show_logic helper2 v
  and helper2: lt gt -> string = function
  | N -> "N"
  | A x -> "A (" ^ (helper x) ^ ")"
  in
  (* show_logic  *)
    helper x

let lt_of_ft cond f : lt =
  let isVar x = cond !!!x in
  let x = !!!(f ()) in
  let rec helper = function
  | N -> Value N
  | A x -> Value (A (helper x))
  in
  if isVar x then (refine_fancy x !!!helper)
  else helper x
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


let show_frt : (rt,rt) fancy -> string = show_fancy @@ show_rt
let show_int = string_of_int
let show_fint : (int,int) fancy -> string = show_fancy @@ string_of_int

let runN printer = runR lt_of_ft printer show_ln;;

let ilist_of_ftyp2 isVar f =
  let cond : 'a -> bool = fun x -> isVar !!!x in
  let rec helper (t: ( (int,int) fancy list as 'l,'l) fancy) : ((int logic, 'l2 logic) llist as 'l2) logic =
    if cond t
    (* then !!!(var_of_fancy !!!t) *)
    then refine_fancy !!!t !!!helper
    else match coerce_fancy !!!t with
    | [] -> Value Nil
    | h :: tl when cond !!!h -> Value (Cons (var_of_fancy !!!h, helper !!!tl))
    | h :: tl -> Value (Cons (Value (cast_fancy !!!h), helper !!!tl))
  in
  helper (Obj.obj @@ f ())

let show1 l  = (GT.show(List.logic) (GT.show(logic) string_of_int)) l
let show_ilist = GT.show(GT.list) @@ GT.show(GT.int)

let show_iflist: (int, int) fancy list -> _ = GT.show(GT.list) @@ (Obj.magic @@ (GT.show(GT.int)))

let runL printer = runR ilist_of_ftyp2 printer show1

let _ =
  runN show_int         (-1) q (REPR (fun q -> (q =/= !1) )         ) qh;
(*
  runN show_frt         (-1) q (REPR (fun q -> (fresh(x) (x =/= (a x)))                                                                 )) qh;
  run_exn show_int       (-1) q
    (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                         )) qh;
  (*run_exn show_list_list (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !1)(![x; y] === q)))) qh; *)
  run_exn show_int       (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !0))               )) qh;
  run_exn show_int       (-1) q (REPR (fun q -> (fresh (x y z)(z === !0)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))               )) qh;
  run_exn show_int       (-1) q (REPR (fun q -> (fresh (x y z)(z === !1)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))               )) qh;
  ()

let _ =
  runN show_int       (-1) q (REPR (fun q -> (fresh (x y z)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                         )) qh;
  runN show_int       (-1) q (REPR (fun q -> (fresh (x y z)(z === !1)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))               )) qh;
  ()
let _ =
  run_exn show_int       (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2))                                            )) qh;
  run_exn show_int       (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(y === !1))                                            )) qh;
  ()
;;*)

  (* run_exn show_int       (-1) q (REPR (fun _ -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1))                                  )) qh; *)
  (* run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (x y)
                                                    (![x; !1] =/= ![!2; y])
                                                    (![x; y] === q))
                                                  )) qh; *)
  ()
                                                     (*;

  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(![x; y] === q))                             )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !9)(![x; y] === q))                   )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5)(d === !6))                         )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1)(![x; y] === q))                   )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === z))                                  )) qh;
  ()
let _ =
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === !5)(![x; z] === q))                  )) qh;
  ()
let _ =
  runL show_iflist      (-1) q (REPR (fun q -> (!3 =/= !4) )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (!3 =/= !3)                                              )) qh;
  run_exn show_int      (-1) q (REPR (fun q -> ((!5 =/= q) &&& (!6 =/= q) &&& (q === !5))               )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5))  )) qh;
  runN show_fint        (-1) q (REPR (fun q -> (fresh (a)(!3 === a)(a =/= !4))                          )) qh;
  runN show_int         (-1) q (REPR (fun q -> ((!4 =/= q) &&& (!3 =/= q))                              )) qh;
  runN show_int         (-1) q (REPR (fun q -> ((q =/= !5) &&& (q =/= !5))                              )) qh;

  runN show_int         (-1) q (REPR (fun q -> (let foo x = fresh (a)(x =/= a) in fresh(a)(foo a))      )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y))                                                     )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> Fresh.two (fun a d -> ?& [ ![a; d] === q; q =/= ![!5; !6] ]) )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> Fresh.two (fun a d -> ?& [ ![a; d] === q; q =/= ![!5; !6];
                                                                          (a === !3) ])                    )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(y =/= x))                                                     )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y)(y =/= x))                                            )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y)(x =/= y))                                            )) qh;
  runN show_int       (-1) q (REPR (fun q -> ((q =/= !5) &&& (!5 =/= q))                  )) qh;


  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(![x; y] =/= ![!5; !6])(x =/= !5))                             )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= !5)(![x; y] =/= ![!5; !6]))                             )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(x =/= !5)(![x; y] =/= ![!5; !6])(![x; y] === q))                             )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(!5 =/= x)(![x; y] =/= ![!5; !6])(![x; y] === q))                             )) qh;
  runL show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(!5 =/= x)(![y; x] =/= ![!6; !5])(![x; y] === q))                             )) qh;
  runL show_iflist      (-1) q (REPR (fun x -> (fresh (y z)(x =/= ![y; !2])(x === ![z; !2]))                                             )) (fun xs -> ["x", xs]);
  ()


let show_int_list = GT.show(GT.list) @@ GT.show(GT.int)
let () =
  let rec distincto l =
    conde [
      l === nil ();
      (fresh (a) (l === !< a));
      (fresh (a ad dd) (
         (l === a % (ad % dd)) &&&
         (a =/= ad) &&&
         (distincto (a % dd)) &&&
         (distincto (ad % dd))
      ))
    ]
   in
   run_exn show_int (-1) q (REPR (fun q -> distincto (!2 % (!3 %< q)))) qh;

   let rec remembero x ls out =
     conde [
       (ls === nil ()) &&& (out === nil ());
       fresh (a d res) (
         (ls === a % d) &&&
         (remembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (out === a % res)
          ])
       )
     ]
   in
   run_exn show_int_list (-1) q (REPR (fun q -> remembero !1 (!1 % (!2 % (!1 %< !3))) q         )) qh;
   runN show_int (-1) q (REPR (fun q -> remembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)))) qh;

   let rec rembero x ls out =
     conde [
       (ls === nil ()) &&& (out === nil ());
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
   run_exn show_int_list (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 % (!1 %< !3))) q         )) qh;
   runN (fun () -> assert false) (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)))) qh;
   ()
*)
