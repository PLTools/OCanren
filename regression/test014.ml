open GT
open MiniKanren
open Tester

@type bin = I | O with show
@type sgn = Pos | Neg with show

@type num = Zero | Num of sgn logic * bin llist logic with show  

(** ADDO **) 

let add_bino x y z a b =
  conde [
    (x === !O) &&& (y === !O) &&& (a === !O) &&& (z === !O) &&& (b === !O);
    (x === !I) &&& (y === !O) &&& (a === !O) &&& (z === !I) &&& (b === !O);
    (x === !O) &&& (y === !I) &&& (a === !O) &&& (z === !I) &&& (b === !O);
    (x === !O) &&& (y === !O) &&& (a === !I) &&& (z === !I) &&& (b === !O);
    (x === !I) &&& (y === !I) &&& (a === !O) &&& (z === !O) &&& (b === !I);
    (x === !I) &&& (y === !O) &&& (a === !I) &&& (z === !O) &&& (b === !I);
    (x === !O) &&& (y === !I) &&& (a === !I) &&& (z === !O) &&& (b === !I);
    (x === !I) &&& (y === !I) &&& (a === !I) &&& (z === !I) &&& (b === !I)
  ]


let rec value_incro a b =
  conde [
    (a === !Nil) &&& (b === !O % !Nil);
    fresh (x xs y ys)
      (a === x % xs)
      (b === y % ys)
      (conde [
        (x === !O) &&& (y === !I) &&& (xs === ys);
        (x === !I) &&& (y === !O) &&& (value_incro xs ys)
      ])
  ]


let rec value_addo a b c r =
  conde [  
    (a === !Nil) &&& (r === !O) &&& (value_incro b c);
    (a === !Nil) &&& (r === !I) &&& (fresh (c') 
      (value_incro b c')
      (value_incro c' c));
    (a =/= !Nil) &&& (b === !Nil) &&& (r === !O) &&& (value_incro a c);
    (a =/= !Nil) &&& (b === !Nil) &&& (r === !I) &&& (fresh (c') 
      (value_incro a c')
      (value_incro c' c));
    fresh (x xs y ys z zs r')
      (a === x % xs)
      (b === y % ys)
      (c === z % zs)
      (add_bino x y z r r')
      (value_addo xs ys zs r')
  ]


let rec addo x y z =
  conde [
    (x === !Zero) &&& (y === z);
    (x =/= !Zero) &&& (y === !Zero) &&& (x === z);
    fresh (s1 v1 s2 v2)
      (x === !(Num (s1, v1)))
      (y === !(Num (s2, v2)))
      (conde [
        (z === !Zero) &&& (s1 =/= s2) &&& (v1 === v2);
        (z =/= !Zero) &&& (s1 === !Neg) &&& (addo z (!(Num (!Pos, v1))) y);
        (s1 === !Pos) &&& (conde [
          (s2 === !Neg) &&& (addo z (!(Num (!Pos, v2))) x);
          fresh (s3 v3)
            (z === !(Num (s3, v3)))
            (s2 === !Pos)
            (s3 === !Pos)
            (value_addo v1 v2 v3 !O) 
        ])
      ])
    ]

(** MULTO **)

let rec value_multo x y z =
  conde [
    (x === !Nil) &&& (z === y);
    (x =/= !Nil) &&& (fresh (x' z')
      (value_incro x' x)
      (value_multo x' y z')
      (value_addo y z' z (!O))) 
  ]

let multo x y z =
  conde [
    ((x === !Zero) ||| (y === !Zero)) &&& (z === !Zero);
    fresh (s1 v1 s2 v2 s3 v3)
      (x === !(Num (s1, v1)))
      (y === !(Num (s2, v2)))
      (z === !(Num (s3, v3)))
      (value_multo v1 v2 v3)
      (conde [
        (s1 === s2) &&& (s3 === !Pos);
        (s1 =/= s2) &&& (s3 === !Neg)
    ])
  ]

(** COMPARE **)

let rec value_lesso x y =
  conde [
    (x === !Nil) &&& (fresh (n ns) (y === n % ns));
    fresh (n ns m ms)
      (x === n % ns)
      (y === m % ms)
      (conde [
        (ns === ms) &&& (n === !O) &&& (m === !I); 
        (value_lesso ns ms)
      ])
  ]

let lesso x y =
  conde [
    (x === !Zero) &&& (fresh (v) (y === !(Num (!Pos, v))));
    (y === !Zero) &&& (fresh (v) (x === !(Num (!Neg, v))));
    (fresh (s1 v1 s2 v2)
      (x === !(Num (s1, v1)))
      (y === !(Num (s2, v2)))
      (conde [
        (s1 === !Neg) &&& (s2 === !Pos);
        (s1 === !Pos) &&& (s2 === !Pos) &&& (value_lesso v1 v2);
        (s1 === !Neg) &&& (s2 === !Neg) &&& (value_lesso v2 v1)
      ])
    )
  ]

(** HELPERS **)

let to_num a =
  let rec to_value a = 
    if a = 1 
      then !Nil
      else
        match a mod 2 with
        | 0 -> !O % to_value (a / 2)
        | 1 -> !I % to_value (a / 2)
        | _ -> !Nil
  in
    if      a = 0 then !Zero
    else if a < 0 then !(Num (!Neg, to_value (-a)))
    else               !(Num (!Pos, to_value ( a)))


(** SHOWS **)

let show_bin = show logic (show bin)
let show_num  = show logic (show num)
let show_bin_llist = show logic (show llist (show bin))

(** TESTS **)

let _ =

  (* addo *)
  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num 0) (to_num 0) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num 1) (to_num 0) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num 0) (to_num 1) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num 1) (to_num 1) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num (-3)) (to_num 3) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num 2) (to_num 3) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (addo (to_num 7) (to_num 7) q st), ["q",
 q]);

  (* multo *)
  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num 10) (to_num 10) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num 0) (to_num 10) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num 10) (to_num 0) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo q (to_num 0) (to_num 0) st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num 0) q (to_num 0) st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num 7) (to_num 3) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num (-7)) (to_num 3) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num 7) (to_num (-3)) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (multo (to_num (-7)) (to_num (-3)) q st), ["q",
 q]);

  (* compare *)
  run show_num  empty_reifier 5 q (fun q st -> REPR (lesso (to_num (0)) q st), ["q",
 q]);

  run show_num  empty_reifier 5 q (fun q st -> REPR (lesso (to_num (-1)) q st), ["q",
 q]);

  run show_num  empty_reifier 100 q (fun q st -> REPR (lesso (to_num (1)) q st), ["q",
 q]);
