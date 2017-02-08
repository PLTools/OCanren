open MiniKanren
open Tester

type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic [@@deriving show { nofullpath = true }]
type typ = V of string logic | Arr of typ logic * typ logic [@@deriving show { nofullpath = true }] 

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

let show_typ    = show_logic (show_typ)
let show_lam    = show_logic (show_lam)
let show_string = show_logic (fun s -> s)
let show_env    = List.show_logic (show_logic (fun (s, t) -> Printf.sprintf "(%s, %s)" (show_string s) (show_typ t)))

let _ =
  run show_typ    1 q (REPR (fun q -> lookupo !"x" (inj_list []) q                                        )) qh;
  run show_typ    1 q (REPR (fun q -> lookupo !"x" (inj_list [!"x", !(V !"x")]) q                         )) qh; 
  run show_typ    1 q (REPR (fun q -> lookupo !"x" (inj_list [!"y", !(V !"y"); !"x", !(V !"x")]) q        )) qh;

  run show_string 1 q (REPR (fun q -> lookupo q (inj_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"x")   )) qh;
  run show_string 1 q (REPR (fun q -> lookupo q (inj_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"y")   )) qh;
  run show_env    1 q (REPR (fun q -> lookupo !"x" q !(V !"y")                                            )) qh;
  run show_env    5 q (REPR (fun q -> lookupo !"x" q !(V !"y")                                            )) qh; 
  run show_typ    1 q (REPR (fun q -> infero !(Abs (!"x", !(X !"x"))) q                                   )) qh;
  run show_typ    1 q (REPR (fun q -> infero !(Abs (!"f", !(Abs (!"x", !(App (!(X !"f"), !(X !"x"))))))) q)) qh;
  run show_typ    1 q (REPR (fun q -> infero !(Abs (!"x", !(Abs (!"f", !(App (!(X !"f"), !(X !"x"))))))) q)) qh;
  run show_lam    1 q (REPR (fun q -> infero q !(Arr (!(V !"x"), !(V !"x")))                              )) qh;
  run show_typ    1 q (REPR (fun q -> infero !(Abs (!"x", !(App (!(X !"x"), !(X !"x")))))                q)) qh
  


