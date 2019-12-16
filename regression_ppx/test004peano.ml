open OCanren
open OCanren.Std

open Tester
open Printf
open GT

module Fmap1 = OCanren.Fmap

module Peano = struct
  type 'a t = O | S of 'a [@@distrib] [@@deriving gt ~options:{show; gmap}]


  type ground = ground T.t
  type lnat = lnat T.t logic

  type rt = rt T.t            (* normal type *)
  type lt = lt T.t logic      (* reified     *)
  type ft = (rt, lt) injected (* injected    *)

  let rec show_ln n = show(logic) (show(t) show_ln) n
  let rec show_rn n = show(t) show_rn n

end


let rec peano_reifier c x = Peano.t_reify peano_reifier c x

let runN n = runR peano_reifier Peano.show_rn Peano.show_ln n

let o      = Peano.o ()
let s prev = Peano.s prev

let rec addo x y z =
  conde [
    (x === o) &&& (z === y);
    Fresh.two (fun x' z' ->
      (x === s x') &&&
      (z === s z') &&&
      (addo x' y z')
    )
  ]

let rec mulo x y z =
  conde [
    (x === o) &&& (z === o);
    fresh (x' z')
      (x === s x') 
      (addo y z' z) 
      (mulo x' y z')
  ]
