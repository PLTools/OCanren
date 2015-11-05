open GT
open MiniKanren
open Tester

@type lam = X of string | App of lam * lam | Abs of string * lam with mkshow
@type typ = V of string | Arr of typ * typ with mkshow

let rec lookupo a g t =
  fresh (a' t' tl) 
    (g === (a', t')::tl)
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])  

let infero expr typ =
  let rec infero gamma expr typ =
    conde [
      (fresh (x)
         (expr === X x)
         (lookupo x gamma typ));
      (fresh (m n t)    
         (expr === App (m, n)) 
         (infero gamma m (Arr (t, typ))) 
         (infero gamma n t));
      (fresh (x l t t') 
         (expr === Abs (x, l)) 
         (typ  === Arr (t, t'))
         (infero ((x, t)::gamma) l t'))
    ]
  in
  infero [] expr typ      

let mkshow_env = mkshow list (mkshow pair (mkshow string) (mkshow typ))

let _ =
  run (mkshow typ)    1 q (fun q st -> REPR (lookupo "x" [] q st), ["q", q]);
  run (mkshow typ)    1 q (fun q st -> REPR (lookupo "x" ["x", V "x"] q st), ["q", q]);
  run (mkshow typ)    1 q (fun q st -> REPR (lookupo "x" ["y", V "y"; "x", V "x"] q st), ["q", q]);
  run (mkshow string) 1 q (fun q st -> REPR (lookupo q ["y", V "y"; "x", V "x"] (V "x") st), ["q", q]);
  run (mkshow string) 1 q (fun q st -> REPR (lookupo q ["y", V "y"; "x", V "x"] (V "y") st), ["q", q]);
  run  mkshow_env     1 q (fun q st -> REPR (lookupo "x" q (V "y") st), ["q", q]);
  run  mkshow_env     5 q (fun q st -> REPR (lookupo "x" q (V "y") st), ["q", q]);
  
  run (mkshow typ)    1 q (fun q st -> REPR (infero (Abs ("x", X "x")) q st), ["q", q]);
  run (mkshow typ)    1 q (fun q st -> REPR (infero (Abs ("f", (Abs ("x", App (X "f", X "x"))))) q st), ["q", q]);
  run (mkshow typ)    1 q (fun q st -> REPR (infero (Abs ("x", (Abs ("f", App (X "f", X "x"))))) q st), ["q", q]);
  
  run (mkshow lam)    1 q (fun q st -> REPR (infero q (Arr (V "x", V "x")) st), ["q", q])
