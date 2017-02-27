open Printf
open MiniKanren
open Tester

@type token = Id | Add | Mul with show;;
let show_token = GT.show(token)

let id  ()  = inj@@lift Id
let add ()  = inj@@lift Add
let mul ()  = inj@@lift Mul;;

module GExpr = struct
  module X = struct
    @type 'self t  = I | A of 'self * 'self | M of 'self * 'self with show;;
    let fmap f = function
    | I -> I
    | A (x,y) -> A (f x, f y)
    | M (x,y) -> M (f x, f y)
  end
  include X
  include Fmap1(X)


  type  expr = expr t
  type lexpr = lexpr t logic
  type fexpr = (expr, lexpr) fancy

  let rec show_expr  e = GT.(show X.t show_expr) e
  let rec show_lexpr e = show_logic GT.(show X.t show_lexpr) e

end

open GExpr
let i ()  : fexpr = inj @@ distrib  I
let a a b : fexpr = inj @@ distrib @@ A (a,b)
let m a b : fexpr = inj @@ distrib @@ M (a,b)

(* let expr = {
  GT.gcata = ();
  GT.plugins =
    object(self)
      (* It's expected a more simpler way to do this to be exists but I can't figure out it at the moment *)
      method show    = function
      | I -> "I ()"
      | A (a,b) -> sprintf "A (%s, %s)" (self#show a) (self#show b)
      | M (a,b) -> sprintf "M (%s, %s)" (self#show a) (self#show b)
  end
} *)

(* let show_expr e = GT.show(expr) e
let rec show_fexpr e = show_fancy show_expr e
let rec show_lexpr e = show_logic GT.(show(gexpr) show_lexpr) e *)


(* let lexpr_of_fexpr (c: var_checker) e =
  let rec helper (t: expr) : lexpr =
    if c#isVar t then refine_fancy (injlift t) c helper
    else match t with
    | I        -> Value I
    | A (a,b)  -> Value (A (helper a, helper b) )
    | M (a,b)  -> Value (M (helper a, helper b) )
    (* TODO: We don't check that pair itself is a fancy value. Can this be possible? *)
  in
  if c#isVar e then refine_fancy e c helper
  else helper (coerce_fancy e) *)


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

let rec pId n n' r = (sym (id()) n n') &&& (r === i())
and pAdd i i' r = (pMulPlusAdd <|> pMul) i i' r
and pMulPlusAdd i i' r = (
      pMul |>
      (fun r i i'' r'' ->
         fresh (r' i')
           (sym (add()) i i')
           (r'' === (a r r'))
           (pAdd i' i'' r')
      )) i i' r
and pMul i i' r = (pIdAstMul <|> pId) i i' r
and pIdAstMul i i' r = (
      pId |>
      (fun r i i'' r'' ->
         fresh (r' i')
           (sym (mul()) i i')
           (r'' === (m r r'))
           (pMul i' i'' r')
      )) i i' r
and pTop i i' r = pAdd i i' r

let pExpr i r = fresh (i') (pTop i i' r) (eof i')

let runE_exn n = run_exn show_expr n
let show_stream xs = GT.show(List.ground) show_token xs

let _ =
  runE_exn   1   q   qh (REPR (fun q -> pExpr (inj_list [id ()]) q                              ));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (inj_list [id (); mul (); id ()]) q               ));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (inj_list [id (); mul (); id (); mul (); id ()]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (inj_list [id (); mul (); id (); add (); id ()]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (inj_list [id (); add (); id (); mul (); id ()]) q));
  runE_exn   1   q   qh (REPR (fun q -> pExpr (inj_list [id (); add (); id (); add (); id ()]) q));
  run_exn show_stream 1   q   qh (REPR (fun q -> pExpr q (m (i ()) (i ()))                   ));
  ()
