open Printf
open GT
open OCanren
open OCanren.Std

module GLam =
  struct
    module T =
      struct
        @type ('varname, 'self) t =
        | V of 'varname
        | App of 'self * 'self
        | Abs of 'varname * 'self
        with show, gmap

        let fmap f g x = gmap(t) f g x
      end

  include T
  include Fmap2(T)

  type rlam = (string, rlam) t
  type llam = (string logic, llam) t logic
  type flam = (rlam, llam) injected

  let v   s   = inj @@ distrib @@ V s
  let app x y = inj @@ distrib @@ App (x,y)
  let abs x y = inj @@ distrib @@ Abs (x,y)

  let rec show_rlam term = show(t) (show(string)) show_rlam term
  let rec show_llam term = show(logic) (show(t) (show(logic) @@ show string)show_llam) term
end

open GLam

let varX = !! "x"
let varY = !! "y"
let varF = !! "f"

let rec glam_reifier : VarEnv.t -> GLam.flam -> GLam.llam = fun c x ->
  GLam.reify OCanren.reify glam_reifier c x
