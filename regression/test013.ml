(*
 * MiniKanren: bool, int and list relations tests
 * Copyright (C) 2016
 * Aleksei Semin, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)

open GT
open MiniKanren
open Tester


(****************************************************************************)
(* relations between logic values *)

(* sheffer stroke *)
let (|^) a b c =
  conde [
    (a === !false) &&& (b === !false) &&& (c === !true);
    (a === !false) &&& (b === !true)  &&& (c === !true);
    (a === !true)  &&& (b === !false) &&& (c === !true);
    (a === !true)  &&& (b === !true)  &&& (c === !false);
  ]

(* logic negation *)
let noto a na = (a |^ a) na

(* logic conjunction *)
let oro a b c = 
  fresh (aa bb)
    ((a  |^ a) aa)
    ((b  |^ b) bb)
    ((aa |^ bb) c)

(* logic disjunction *)
let ando a b c = 
  fresh (ab)
    ((a  |^ b) ab)
    ((ab |^ ab) c)

;;

(****************************************************************************)
(* natural numbers *)

@type nat = O | S of nat logic with show


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

(****************************************************************************)
(* polymorphic list relations *)

let rec foldro f a xs r =
  conde [
    (xs === !Nil) &&& (a === r);
    fresh (h t a')
      (xs === h % t)
      (f h a' r)
      (foldro f a t a')
  ]

let rec mapo f xs ys =
  conde [
    (xs === !Nil) &&& (ys === !Nil);
    fresh (z zs)
      (xs === z % zs)
      (fresh (a1 a2)
        (f z a1)
        (mapo f zs a2)
        (ys === a1 % a2)
      )
  ]

let filtero p xs ys =
  let folder x a a' =
    conde [
      (p x !true) &&& (x % a === a');
      (p x !false) &&& (a === a')
    ]
  in
    foldro folder !Nil xs ys

let rec lookup' p = function
| [] -> None
| x::xs -> if p x then Some x else lookup' p xs

let rec lookup p xs mx =
  conde [
    (xs === !Nil) &&& (mx === !None);
    fresh(h t )
      (h % t === xs)
      (conde [
        (p h !true) &&& (mx === !(Some h));
        (p h !false) &&& (lookup p t mx)
      ])
  ]


(****************************************************************************)
(* bools and lists *)

let anyo = foldro oro  !false

let allo = foldro ando !true

(****************************************************************************)
(* nats and lists *)

let sumo = foldro addo ?$0

let lengtho xs n =
  let folder x a a' =
    (addo ?$1 a a')
  in 
    foldro folder ?$0 xs n



(****************************************************************************)
(* show for complex logic types *)

let show_bool      = show logic (show bool)
let show_nat       = show logic (show nat)
let show_int       = show logic (show int)
let show_bool_list = show logic (show list show_bool)
let show_int_list  = show logic (show list show_int)
let show_nat_list  = show logic (show list show_nat)
let show_bool_llist = show logic (show llist (show bool))
let show_int_llist = show logic (show llist (show int))
let show_nat_llist = show logic (show llist (show nat))
let show_option t = show logic (show option (show logic (show t)))


(****************************************************************************)
(* reifiers *)

let int_reifier dc x =
  match reify dc x with
  | [] -> ""
  | cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_int x) (show_int c)) cs

let int_list_reifier dc t = 
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
  | cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_int_list x) (show_int_list c)) cs
      end
  | Value l -> 
      List.fold_left (fun acc -> function (Var i) as x when not (check_var i) -> add_var i; acc ^ int_reifier dc x | _ -> acc) "" l


(****************************************************************************)
(* tests *)

let _ =
  (* logic values *)

  (* not true  == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (noto !true  q           st), ["q", q]);

  (* not false == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (noto !false q           st), ["q", q]);

  (* not ?     == true *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (noto q      !true       st), ["q", q]);

  (* or  x y == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (oro  !false !false q    st), ["q", q]);
  run show_bool empty_reifier 1 q (fun q st -> REPR (oro  !false !true  q    st), ["q", q]);
  run show_bool empty_reifier 1 q (fun q st -> REPR (oro  !true  !false q    st), ["q", q]);
  run show_bool empty_reifier 1 q (fun q st -> REPR (oro  !true  !true  q    st), ["q", q]);

  (* and x y == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (ando !false !false q    st), ["q", q]);
  run show_bool empty_reifier 1 q (fun q st -> REPR (ando !false !true  q    st), ["q", q]);
  run show_bool empty_reifier 1 q (fun q st -> REPR (ando !true  !false q    st), ["q", q]);
  run show_bool empty_reifier 1 q (fun q st -> REPR (ando !true  !true  q    st), ["q", q]);


  (****************************************************************************)
  (* nat values *)

  (* add 0 1 == ? *)
  run show_nat empty_reifier 1  q (fun q st   -> REPR (addo ?$0 ?$1 q   st), ["q", q]);
  (* add 1 ? == 3 *)
  run show_nat empty_reifier 1  q (fun q st   -> REPR (addo ?$1 q   ?$3 st), ["q", q]);
  (* add q ? == q *)
  run show_nat empty_reifier 3 qr (fun q r st -> REPR (addo q   r   q   st), ["q", q; "r", r]);
  (* mul 1 2 == ? *)
  run show_nat empty_reifier 1  q (fun q st   -> REPR (mulo ?$1 ?$2 q   st), ["q", q]);
  (* mul 3 ? == 6 *)
  run show_nat empty_reifier 1  q (fun q st   -> REPR (mulo ?$3 q   ?$6 st), ["q", q]);
  (* mul 3 0 == ? *)
  run show_nat empty_reifier 1  q (fun q st   -> REPR (mulo ?$3 ?$0 q   st), ["q", q]);
  (* mul q ? == q *)
  run show_nat empty_reifier 3 qr (fun q r st -> REPR (mulo q   r   q   st), ["q", q; "r", r]);
  (* mul ? 5 == 0 *)
  run show_nat empty_reifier 1  q (fun q st   -> REPR (mulo q   ?$5 ?$0 st), ["q", q]);
  (* mul ? 0 == 0 *)
  run show_nat empty_reifier 3  q (fun q st   -> REPR (mulo q   ?$0 ?$0 st), ["q", q]);

(*   (* even ? *)
  run show_nat empty_reifier 3  q (fun q st   -> REPR (eveno q st), ["q", q]);
  (* odd  ? *)
  run show_nat empty_reifier 3  q (fun q st   -> REPR (oddo  q st), ["q", q]);
 *)

  (****************************************************************************)
  (* foldr *)

  (* sum [] == ? *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (sumo (of_list []) q st), ["q", q]); 
  (* sum [3,2,1] == ? *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (sumo (nats [3;1;2]) q st), ["q", q]); 
  (* sum [0,1,?,3] == 6 *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (sumo (?$0 % (?$1 % (q %< ?$3))) ?$6 st), ["q", q]); 

  (* length [1,2,3,4] == ? *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (lengtho (of_list [1;2;3;4]) q st), ["q", q]); 
  (* length [1,2,3,4] == ? *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (lengtho (of_list [(); (); ()]) q st), ["q", q]); 
  (* length [false, true] == ? *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (lengtho (of_list [false; true]) q st), ["q", q]); 
  (* length [4,3,2,1,0] == ? *)
  run show_nat empty_reifier 1 q (fun q st -> REPR (lengtho (nats [4;3;2;1;0]) q st), ["q", q]); 
  (* length ? == 3 *)
  run show_nat_llist empty_reifier 1 q (fun q st -> REPR (lengtho q ?$3 st), ["q", q]); 
  (* length ? == 0 *)
  run show_nat_llist empty_reifier 1 q (fun q st -> REPR (lengtho q ?$0 st), ["q", q]); 


  (* any [false, false, true] == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (anyo (of_list [false;false;true]) q st), ["q", q]); 

  (* any [false, false] == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (anyo (of_list [false;false]) q st), ["q", q]); 

  (* all [true, false, true] == ? *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (allo (of_list [true;false;true]) q st), ["q", q]); 

  (* all [true, ?, true] == true *)
  run show_bool empty_reifier 1 q (fun q st -> REPR (allo (!true % (q %< !true)) !true st), ["q", q]); 

  (* all [true, ?, ?] == ? *)
  run show_bool empty_reifier (-1) qrs (fun q r s st -> REPR (allo (!true % (q %< r)) s st), ["q", q; "r", r; "s", s]); 


  (****************************************************************************)
  (* map *)

  (* map (add 1) [0,1,2] ?       *)
  run show_nat_llist empty_reifier 1  q (fun q st   -> REPR (mapo (addo ?$1) (nats [0;1;2]) q            st) , ["q", q]);

  (* map (add 2) ?       [4,3,2] *)
  run show_nat_llist empty_reifier 1  q (fun q st   -> REPR (mapo (addo ?$2) q (nats [4;3;2])            st) , ["q", q]);

  (* map (add ?) [1,2,3] [4,5,6] *)
  run show_nat empty_reifier       1  q (fun q st   -> REPR (mapo (addo q) (nats [1;2;3]) (nats [4;5;6]) st) , ["q", q]);

  (* map (mul ?) [1,2,3] [4,5,6] *)
  run show_nat empty_reifier       1  q (fun q st   -> REPR (mapo (mulo q) (nats [1;2;3]) (nats [2;4;6]) st) , ["q", q]);

  (* map (mul ?) [1,2]   [2,?] *)
  run show_nat empty_reifier       1 qr (fun q r st -> REPR (mapo (mulo q) (nats [1;2]) (?$2 %< r)       st) , ["q", q; "r", r]);

  (* map id [1,2,3] == ? *)
  run show_int_llist empty_reifier 1 q (fun q st -> REPR (mapo (===) (of_list [1;2;3]) q st), ["q", q]); 

  (* map id [1,2,3] == [1,2,?] *)
  run show_int empty_reifier 1 q (fun q st -> REPR (mapo (===) (of_list [1;2;3]) (!1 % (!2 %< q)) st), ["q", q]); 

  (* map not [true, false, true] == ? *)
  run show_bool_llist empty_reifier 1 q (fun q st -> REPR (mapo noto (of_list [true;false;true;]) q st), ["q", q]);

  (* map not [] == ? *)
  run show_bool_llist empty_reifier 1 q (fun q st -> REPR (mapo noto (of_list []) q st), ["q", q]);


  (****************************************************************************)
  (* filter *)

  let eqo x y t =
    conde [
      (x === y) &&& (t === !true);
      (x =/= y) &&& (t === !false);
    ]
  in
  let neqo x y t =
    conde [
      (x =/= y) &&& (t === !true);
      (x === y) &&& (t === !false);
    ]
  in

  run show_nat_llist empty_reifier (-1) q (fun q st -> REPR (filtero (eqo ?$2) (nats [0;1;2;3]) q st), ["q", q]); 

(*   run show_nat_llist empty_reifier 4 q (fun q st -> REPR (filtero eveno (nats [0;1;2;3]) q st), ["q", q]); 

  run show_nat_llist empty_reifier 4 q (fun q st -> REPR (filtero oddo  (nats [0;1;2;3]) q st), ["q", q]); 

  run show_nat_llist empty_reifier 4 q (fun q st -> REPR (filtero eveno q (of_list []) st), ["q", q]); 

  run show_nat_llist empty_reifier 4 q (fun q st -> REPR (filtero eveno q (nats [2]) st), ["q", q]); 
 *)

  run (show_option nat) empty_reifier 1 q (fun q st -> REPR (lookup (eqo ?$1) (nats [0;2;1;3]) q st), ["q", q]);




