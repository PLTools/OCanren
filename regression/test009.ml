open Printf
open GT
open MiniKanren
open Std
open Tester

@type token = Id | Add | Mul with show

let show_token = show(token)

module GExpr =
  struct

    module T =
      struct
        @type 'self t  = I | A of 'self * 'self | M of 'self * 'self with show, gmap

        let fmap f x = gmap(t) f x
     end

  include T
  include Fmap (T)

  type  expr = expr t
  type lexpr = lexpr t logic
  type fexpr = (expr, lexpr) injected

  let rec show_expr  e = show T.t show_expr e
  let rec show_lexpr e = show(logic) (show T.t show_lexpr) e

end

open GExpr

let i ()  : fexpr = inj @@ distrib  I
let a a b : fexpr = inj @@ distrib @@ A (a,b)
let m a b : fexpr = inj @@ distrib @@ M (a,b)

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

let runE_exn n = run_exn show_expr n
let show_stream xs = show(List.ground) show_token xs

let _ =
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id]) q                  ));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id]) q         ));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id; Mul; Id]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id; Add; Id]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Add; Id; Mul; Id]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Add; Id; Add; Id]) q));
  run_exn show_stream 1   q   qh (REPR (fun q -> pExpr q (m (i ()) (i ()))        ));
  ()
