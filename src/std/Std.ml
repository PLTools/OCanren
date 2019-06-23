open Logic
open Core

let eqo x y t =
  conde [
    (x === y) &&& (t === LBool.truo);
    (x =/= y) &&& (t === LBool.falso);
  ]

let neqo x y t =
  conde [
    (x =/= y) &&& (t === LBool.truo);
    (x === y) &&& (t === LBool.falso);
  ]

let nat n = LNat.nat (LNat.of_int n)

let (%)  = LList.cons
let (%<) = LList.(%<)
let (!<) = LList.(!<)
let nil  = LList.nil

let rec list f = function
| []    -> nil ()
| x::xs -> LList.cons (f x) (list f xs)

let rec nat_list = function
| []    -> nil ()
| x::xs -> nat x % nat_list xs

let some = LOption.some
let none = LOption.none
let pair = LPair.pair
