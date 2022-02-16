open Printf
open GT
open OCanren
open OCanren.Std

module GLam = struct


  @type ('varname, 'self) t =
        | V of 'varname
        | App of 'self * 'self
        | Abs of 'varname * 'self
    with show, gmap

  let fmap f g x = gmap(t) f g x


  type ground = (string, ground) t
  type llam = (string logic, llam) t logic
  type injected = (string ilogic, injected) t ilogic

  let v   s   = inj @@ V s
  let app x y = inj @@ App (x,y)
  let abs x y = inj @@ Abs (x,y)

  let rec show_rlam term = show(t) (show(string)) show_rlam term
  let rec show_llam term = show(logic) (show(t) (show(logic) @@ show string)show_llam) term

  let reify : (injected, llam) Reifier.t =
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun fself ->
      Reifier.compose Reifier.reify
        (Reifier.reify >>= fun rstring ->
        fself >>= fun fr ->
        let rec foo = function
          | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
          | Value x -> Value (GT.gmap t rstring fr x)
        in
        Env.Monad.return foo
    ))

  let prj_exn : (injected, ground) Reifier.t =
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun self ->
      Reifier.compose Reifier.prj_exn
      ( self >>= fun fr ->
        Reifier.prj_exn >>= fun rstring ->
        Env.Monad.return (fun x -> GT.gmap t rstring fr x))
      )
end

open GLam

let varX = !! "x"
let varY = !! "y"
let varF = !! "f"
