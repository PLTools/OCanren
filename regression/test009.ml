open Printf
open GT
open OCanren
open OCanren.Std
open Tester

@type token = Id | Add | Mul with show

let show_token = show(token)

module GExpr = struct


  @type 'self t  = I | A of 'self * 'self | M of 'self * 'self
    with show, gmap

  let fmap f x = gmap(t) f x

  type  expr = expr t
  type lexpr = lexpr t logic
  type fexpr = fexpr t ilogic

  let rec show_expr  e = show t show_expr e
  let rec show_lexpr e = show(logic) (show t show_lexpr) e

  let reify : (fexpr, lexpr) Reifier.t =
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun fself ->
      Reifier.compose Reifier.reify
        (fself >>= fun fr ->
        let rec foo = function
          | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
          | Value x -> Value (GT.gmap t fr x)
        in
        Env.Monad.return foo
    ))

  let prj_exn : (fexpr, expr) Reifier.t =
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun self ->
      Reifier.compose Reifier.prj_exn
      ( self >>= fun fr ->
        Env.Monad.return (fun x -> GT.gmap t fr x))
      )
end

open GExpr

let i ()  : fexpr = inj @@ I
let a a b : fexpr = inj @@ A (a,b)
let m a b : fexpr = inj @@ M (a,b)

let sym t i i' =
  fresh (x xs)
    (i === x%xs) (t === x) (i' === xs)

let eof i = i === nil ()

let (|>) x y = fun i i'' r'' ->
  fresh (i' r')
    (x i  i' r')
    (y r' i' i'' r'')

let (<|>) x y = fun i i' r ->
  conde [x i i' r; y i i' r]

let rec pId n n' r = (sym !!Id n n') &&& (r === i())
and pAdd i i' r = (pMulPlusAdd <|> pMul) i i' r
and pMulPlusAdd i i' r = (
      pMul |>
      (fun r i i'' r'' ->
         fresh (r' i')
           (sym !!Add i i')
           (r'' === (a r r'))
           (pAdd i' i'' r')
      )) i i' r
and pMul i i' r = (pIdAstMul <|> pId) i i' r
and pIdAstMul i i' r = (
      pId |>
      (fun r i i'' r'' ->
         fresh (r' i')
           (sym !!Mul i i')
           (r'' === (m r r'))
           (pMul i' i'' r')
      )) i i' r
and pTop i i' r = pAdd i i' r

let pExpr i r = fresh (i') (pTop i i' r) (eof i')

let runE_exn n = run_r GExpr.prj_exn GExpr.show_expr n
let show_stream xs = show(List.ground) show_token xs
let run_stream n = run_r (List.prj_exn OCanren.prj_exn) show_stream n

let _ =
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id]) q                  ));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id]) q         ));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id; Mul; Id]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id; Add; Id]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Add; Id; Mul; Id]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Add; Id; Add; Id]) q));
  run_stream 1   q   qh (REPR (fun q -> pExpr q (m (i ()) (i ()))        ));
  ()
