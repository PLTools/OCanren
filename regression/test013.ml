open GT
open MiniKanren
open Tester

(* natural numbers *)

@type nato = O | S of nato logic with show

let rec (!$) n = if (n <= 0) then O else (S !(!$(n-1)))
let (?$) n = !(!$n)
let nats xs = of_list (List.map (!$) xs) 

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


(* polymorphic list relations *)

let rec foldro f a xs r =
  conde [
    (xs === !Nil) &&& (a === r);
    fresh (h t a')
      (xs === h % t)
      (foldro f a t a')
      (f h a' r)
  ]

let mapo f xs ys =
  let folder x a a' =
    fresh (y)
      (f x y)
      (y % a === a')
  in
    foldro folder !Nil xs ys 

let filtero p xs ys =
  let folder x a a' =
    conde [
      (p x) &&& (x % a === a');
      (a === a')
    ]
  in
    foldro folder !Nil xs ys

(* natural numbers and lists *)

let lengtho xs n =
  let folder x a a' =
    (addo ?$1 a a')
  in 
    foldro folder ?$0 xs n

let rec eveno n = 
  conde [
    (n === !O);
    fresh (k)
      (oddo k)
      (addo ?$1 k n)
  ]
and oddo n =
  fresh (k)
    (eveno k)
    (addo ?$1 k n)

(* sheffer stroke *)
(* let (|^) a b c =
  conde [
    (a === !false) &&& (b === !false) &&& (c === !true);
    (a === !false) &&& (b === !true)  &&& (c === !true);
    (a === !true)  &&& (b === !false) &&& (c === !true);
    (a === !true)  &&& (b === !true)  &&& (c === !false);
  ]

 *)

(* let noto a na = (a |^ a) na *)

(* let oro a b c = 
  fresh (aa bb)
    ((a  |^ a) aa)
    ((b  |^ b) bb)
    ((aa |^ bb) c)

let ando a b c = 
  fresh (ab)
    ((a  |^ b) ab)
    ((ab |^ ab) c)
 *)


(* let show_bool      = show(logic) (show bool) *)
let show_nat       = show logic (show nato)
let show_int       = show logic (show int)
let show_int_list  = show logic (show list show_int)
let show_nat_list  = show logic (show list show_nat)
let show_llist     = show logic (show llist (show int))

(* let int_reifier dc x =
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
 *)

let _ =
  (* run show_bool empty_reifier 1 q (fun q st -> REPR (noto (!true)  q st), ["q", q]); *)
  (* run show_bool empty_reifier 1 q (fun q st -> REPR (noto (!false) q st), ["q", q]); *)

  run show_llist empty_reifier (-1) q (fun q st -> REPR (mapo (===) (of_list [1;2;3]) q st), ["q", q]); 
  run show_int   empty_reifier (-1) q (fun q st -> REPR (mapo (===) (of_list [1;2;3]) (!1 % (!2 %< q)) st), ["q", q]); 

  run show_nat empty_reifier 1 q (fun q st -> REPR (addo ?$0 ?$1 q st), ["q", q]);
  run show_nat empty_reifier 1 q (fun q st -> REPR (addo ?$1 ?$2 q st), ["q", q]);
  run show_nat empty_reifier 1 q (fun q st -> REPR (mulo ?$1 ?$2 q st), ["q", q]);
  run show_nat empty_reifier 1 q (fun q st -> REPR (mulo ?$3 ?$2 q st), ["q", q]);
  run show_nat empty_reifier 1 q (fun q st -> REPR (mulo ?$3 ?$0 q st), ["q", q]);

  let add_one = addo ?$1 
  in
  run show_nat empty_reifier 1 q (fun q st -> REPR (add_one ?$0 q st), ["q", q]);
  run show_nat empty_reifier 1 q (fun q st -> REPR (add_one q ?$5 st), ["q", q]);
  run show_nat empty_reifier 1 q (fun q st -> REPR (foldro addo ?$0 !<(?$3) q st), ["q", q]); 
  run show_nat empty_reifier 1 q (fun q st -> REPR (foldro addo ?$0 (nats [1;2;3]) q st), ["q", q]); 
  run show_nat empty_reifier 1 q (fun q st -> REPR (foldro addo ?$0 (?$ 0 % (?$1 % (q %< ?$3))) ?$6 st), ["q", q]); 


  run show_nat empty_reifier 1 q (fun q st -> REPR (lengtho (of_list [1;2;3;4]) q st), ["q", q]); 
  run show_nat empty_reifier 1 q (fun q st -> REPR (lengtho (of_list [false; true]) q st), ["q", q]); 

  (* run show_nat_list empty_reifier 1 q (fun q st -> REPR (filtero eveno (nats [0;1;2;3;4;5]) q st), ["q", q]);  *)
  (* run show_nat_list empty_reifier 1 q (fun q st -> REPR (filtero oddo  (nats [0;1;2;3;4;5]) q st), ["q", q]);  *)


  (* run show_llist list_reifier     3 q (fun q st -> REPR (mapo (=/=) (of_list [1;2;3]) q st), ["q", q]);  *)




