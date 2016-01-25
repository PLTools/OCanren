open GT
open MiniKanren
open Tester

@type ('var, 'self) alam = X of 'var | App of 'self * 'self | Abs of 'var * 'self with gmap, show
@type ('var, 'self) atyp = V of 'var | Arr of 'self * 'self with gmap, show

(*
Alas, this kind of definitions not supported yet by GT:

  @type lam = (string logic, lam logic) alam with show
  @type typ = (string logic, typ logic) atyp with show
*)
type lam = (string logic, lam logic) alam
type typ = (string logic, typ logic) atyp

let show_string    = show logic (show string)
let rec show_typ t = show logic (show atyp show_string show_typ) t
let rec show_lam l = show logic (show alam show_string show_lam) l
let show_env       = show logic (show llist (show pair show_string show_typ))

let rec lookupo a g t =
  fresh (a' t' tl) 
    (g === !(a', t')%tl)
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])  

let infero expr typ =
  let rec infero gamma expr typ =
    conde [
      fresh (x)
        (expr === !(X x))
        (lookupo x gamma typ);
      fresh (m n t)    
        (expr === !(App (m, n))) 
        (infero gamma m !(Arr (t, typ))) 
        (infero gamma n t);
      fresh (x l t t') 
        (expr === !(Abs (x, l))) 
        (typ  === !(Arr (t, t')))
        (infero (!(x, t)%gamma) l t')
    ]
  in
  infero !Nil expr typ      

let _ =
  run show_typ    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" (of_list []) q                                          st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" (of_list [!"x", !(V !"x")]) q                           st), ["q", q]); 
  run show_typ    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) q          st), ["q", q]);

  run show_string empty_reifier 1 q (fun q st -> REPR (lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"x")     st), ["q", q]);
  run show_string empty_reifier 1 q (fun q st -> REPR (lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"y")     st), ["q", q]);
  run show_env    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" q !(V !"y")                                             st), ["q", q]);
  run show_env    empty_reifier 5 q (fun q st -> REPR (lookupo !"x" q !(V !"y")                                             st), ["q", q]); 
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"x", !(X !"x"))) q                                    st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"f", !(Abs (!"x", !(App (!(X !"f"), !(X !"x"))))))) q st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"x", !(Abs (!"f", !(App (!(X !"f"), !(X !"x"))))))) q st), ["q", q]);
  run show_lam    empty_reifier 1 q (fun q st -> REPR (infero q !(Arr (!(V !"x"), !(V !"x")))                               st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"x", !(App (!(X !"x"), !(X !"x")))))                q st), ["q", q])
