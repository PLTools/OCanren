open Printf
open GT
open MiniKanren

module GLam = struct
  module X = struct
    @type ('varname, 'self) t =
    | V of 'varname
    | App of 'self * 'self
    | Abs of 'varname * 'self
    with show;;
    let fmap f g = function
    | V s -> V(f s)
    | App (x, y) -> App (g x, g y)
    | Abs (s, t) -> Abs (f s, g t)
  end
  include X
  include Fmap2(X)

  type rlam = (string, rlam) t
  type llam  = (string logic, llam) t logic
  type flam = (rlam, llam) injected

  let v   s   = inj @@ distrib @@ V s
  let app x y = inj @@ distrib @@ App (x,y)
  let abs x y = inj @@ distrib @@ Abs (x,y)

  let rec show_rlam term = show(t) (show(string)) show_rlam term
  let rec show_llam term = show(logic) (show(t) (show(logic) @@ show string)show_llam) term
end


open GLam

let varX : (string, string logic) injected = inj (lift "x")
let varY = inj@@lift "y"
let varF = inj@@lift "f"

let rec glam_reifier : var_checker -> GLam.flam -> GLam.llam = fun c x ->
  GLam.reifier ManualReifiers.string_reifier glam_reifier c x
