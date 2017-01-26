open Printf
open MiniKanren
open Tester

@type token = Id | Add | Mul with show;;
let show_token t = GT.show(token) t
(* TODO: We need extra unit -> here because we can mutate fancy values internally, right?  *)
let id  () : (token,token) fancy = inj@@lift Id
let add () : (token,token) fancy = inj@@lift Add
let mul () : (token,token) fancy = inj@@lift Mul;;

@type 'self gexpr  = I | A of 'self * 'self | M of 'self * 'self with show;;
type expr  = expr gexpr
type fexpr = (fexpr gexpr, expr) fancy
type lexpr = lexpr gexpr logic

let expr = {
  GT.gcata = ();
  GT.plugins =
    object(self)
      (* It's expected a more simpler way to do this to be exists but I can't figure out it at the moment *)
      method show    = function
      | I -> "I ()"
      | A (a,b) -> sprintf "A (%s, %s)" (self#show a) (self#show b)
      | M (a,b) -> sprintf "M (%s, %s)" (self#show a) (self#show b)
  end
}

let show_expr e = GT.show(expr) e

(* Once again it is needed only because extra 2nd type parameter of fancy type *)
module ExprHack = FMapALike0(struct
  type t = fexpr gexpr
  type r = expr
end)
(* TODO: maybe add primitive wrapinjlift to the functor application result *)
let i ()  : fexpr = ExprHack.wrap @@ inj @@ lift I
let a a b : fexpr = ExprHack.wrap @@ inj @@ lift (A (a,b))
let m a b : fexpr = ExprHack.wrap @@ inj @@ lift (M (a,b))

let lexpr_of_fexpr (c: var_checker) f =
  let rec helper (t: fexpr) : lexpr =
    if c#isVar t then refine_fancy_var t c helper
    else match coerce_fancy t with
    | I        -> Value I
    | A (a,b)  -> Value (A (helper a, helper b) )
    | M (a,b)  -> Value (M (helper a, helper b) )
  in
  helper f

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

(* let show_token  = show(logic) (show token)
let show_expr   = show(logic) (show expr)
let show_stream = show(List.logic) (show(logic) (show token)) *)

let runE_exn n = run_exn show_expr n
let show_stream xs = GT.show(GT.list) show_token xs

let (b000: (token list -> string) -> unit) =
   fun p -> run_exn p 1 q (REPR (fun q -> pExpr q (m (i())  (i()))                   )) qh

let _ =
  runE_exn   1 q (REPR (fun q -> pExpr (inj_list [id ()]) q                  )) qh;
  runE_exn   1 q (REPR (fun q -> pExpr (inj_list [id (); mul (); id ()]) q         )) qh;
  runE_exn   1 q (REPR (fun q -> pExpr (inj_list [id (); mul (); id (); mul (); id ()]) q)) qh;
  runE_exn   1 q (REPR (fun q -> pExpr (inj_list [id (); mul (); id (); add (); id ()]) q)) qh;
  runE_exn   1 q (REPR (fun q -> pExpr (inj_list [id (); add (); id (); mul (); id ()]) q)) qh;
  runE_exn   1 q (REPR (fun q -> pExpr (inj_list [id (); add (); id (); add (); id ()]) q)) qh;
  run_exn show_stream 1 q (REPR (fun q -> pExpr q (m (i ()) (i ()))                   )) qh;
  ()
