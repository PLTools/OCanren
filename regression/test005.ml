open Printf
open MiniKanren
open Tester

type 'a f = ('a, 'a) fancy
type ('varname, 'self) glam =
  | V of 'varname
  | App of 'self * 'self
  | Abs of 'varname * 'self
  (* | MetaVar *)

type lam = (string, lam) glam
type flam = ((string f, flam) glam, lam) fancy
type llam = ((string logic, llam) glam) logic

(* reifier for lambdas  *)
let llam_of_flam isVar f =
  let cond : (_,_) fancy -> bool = fun x -> isVar !!!x in
  let rec helper (t: flam) : llam =
    if cond t then refine_fancy2 t isVar
    else match coerce_fancy t with
    | V s -> Value (if cond s then V (refine_fancy2 s isVar) else V (Value (coerce_fancy s)))
    | App (f,g) ->
      Value (App ((if cond f then refine_fancy2 f isVar else helper f),
                  (if cond g then refine_fancy2 g isVar else helper g)))
    | Abs (n, body) ->
      Value (App ( Value (if cond n then V (refine_fancy2 n isVar) else V (Value (coerce_fancy n))) ,
                   (if cond body then refine_fancy2 body isVar else helper body)
                 )
            )
  in
  helper f


module LamHack = FMapALike0(struct
  type t = (string f, flam) glam
  type r = lam
end)

let v x   : flam = LamHack.wrap @@ inj @@ lift @@ V x
let app x y = LamHack.wrap @@ inj @@ lift @@ App (x,y)
let abs x y = LamHack.wrap @@ inj @@ lift @@ Abs (x,y)

let show_lam lam =
  let b = Buffer.create 10 in
  let rec helper = function
  | V s -> bprintf b "V %s" s
  | App (f,arg) ->
    bprintf b "App (";
    helper f;
    bprintf b ", ";
    helper arg;
    bprintf b ")"
  | Abs (s, e) ->
    bprintf b "Abs (%s, " s;
    helper e;
    bprintf b ")"
  (* | MetaVar -> bprintf b "_.%d" 0 *)
  in
  helper lam;
  Buffer.contents b
;;

let show_flam lam =
  let b = Buffer.create 10 in
  let rec helper = function
  | V s -> bprintf b "V "; bprintf_fancy b (bprintf b "\"%s\"") s
  | App (f,m) ->
    bprintf b "App (";
    bprintf_fancy b helper f;
    bprintf b ", ";
    bprintf_fancy b helper m;
    bprintf b ")"
  | Abs (s, e) ->
    bprintf b "Abs (";
    bprintf_fancy b (bprintf b "%s") s;
    bprintf b ", ";
    bprintf_fancy b helper e;
    bprintf b ")"
  (* | MetaVar -> bprintf b "_.%d" 0 *)
  in
  helper lam;
  Buffer.contents b
;;

let show_llam lam =
  let b = Buffer.create 10 in
  let rec helper = function
  | V s -> bprintf b "V "; bprintf_logic b (bprintf b "\"%s\"") s
  | App (f,m) ->
    bprintf b "App (";
    bprintf_logic b helper f;
    bprintf b ", ";
    bprintf_logic b helper m;
    bprintf b ")"
  | Abs (s, e) ->
    bprintf b "Abs (";
    bprintf_logic b (bprintf b "%s") s;
    bprintf b ", ";
    bprintf_logic b helper e;
    bprintf b ")"
  (* | MetaVar -> bprintf b "_.%d" 0 *)
  in
  bprintf_logic b helper lam;
  Buffer.contents b
;;

let (_:llam -> string) = show_llam;;
let (_:lam -> string) = show_lam;;
(************************************************************************************************************)
@type ('a, 'b) gtyp =
  (* primitive *)
  | P of 'a
  | Arr of 'b * 'b with gmap;;

type typ  = (string, typ) gtyp
type ftyp = ((string f, ftyp) gtyp, typ) fancy
type ltyp = (string logic, ltyp) gtyp logic

module TypFamilies = FMapALike0(struct
  type f1 = (string f, ftyp) gtyp
  type t = f1
  type r = typ
end)
let (_: ((string f, ftyp) gtyp, (string f, ftyp) gtyp) fancy ->
        ((string f, ftyp) gtyp,                   typ) fancy)
  = TypFamilies.wrap

(* type intf = (int,int) fancy *)
(* let (_: ftyp -> ftyp ->ftyp) = fun x y -> inj @@ lift (Arr (x,y)) *)

let p s     : ftyp = TypFamilies.wrap @@ inj @@ lift @@ P s
let arr x y : ftyp = TypFamilies.wrap @@ inj @@ lift @@ Arr (x,y)

(* reifier for types *)
let ltyp_of_ftyp isVar f =
  let cond : (_,_) fancy -> bool = fun x -> isVar !!!x in
  let rec helper (t: ftyp) : ltyp =
    if cond t then refine_fancy2 t isVar
    else match coerce_fancy t with
    | P s -> Value (if cond s then P (refine_fancy2 s isVar) else P (Value (coerce_fancy s)))
    | Arr (f,g) ->
      Value (Arr ((if cond f then refine_fancy2 f isVar else helper f),
                  (if cond g then refine_fancy2 g isVar else helper g)))
  in
  helper f

let (_: (Obj.t -> bool) -> ftyp -> ltyp) = ltyp_of_ftyp

let show_typ typ =
  let b = Buffer.create 10 in
  let rec helper = function
  | P s -> bprintf b "%s" s
  | Arr (f,m) ->
    helper f;
    bprintf b " -> ";
    helper m
  in
  helper typ;
  Buffer.contents b

let show_ftyp typ =
  (* printf "show_typ '%s'\n%!" (generic_show typ); *)
  let b = Buffer.create 10 in
  let rec helper = function
  | P s -> bprintf_fancy b (bprintf b "%s") s
  | Arr (f,m) ->
    bprintf_fancy b helper f;
    bprintf b " -> ";
    bprintf_fancy b helper m
  in
  bprintf_fancy b helper typ;
  Buffer.contents b

let show_ltyp typ =
  (* printf "show_lyp '%s'\n%!" (generic_show typ); *)
  let b = Buffer.create 10 in
  let rec helper = function
  | P s -> bprintf_logic b (bprintf b "%s") s
  | Arr (f,m) ->
    bprintf_logic b helper f;
    bprintf b " -> ";
    bprintf_logic b helper m
  in
  bprintf_logic b helper typ;
  Buffer.contents b


(*
let typ_reifier (cond: Obj.t -> bool) (x : typ) : typ =
  let rec helper x =
    if cond @@ Obj.repr x then TypeVar 0
    else match Obj.magic x with
    | TypeVar n -> TypeVar n
    | Arr (f,x) -> Arr(helper f, helper x)
    | P s when cond @@ Obj.repr s -> TypeVar 0
    | P s -> P s
  in
  helper x
*)
let rec lookupo a g t =
  Fresh.three (fun a' t' tl ->
    (g === (inj_pair a' t')%tl) &&&
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])
  )

let infero expr typ =
  let rec infero gamma expr typ =
    conde [
      Fresh.one (fun x ->
        (expr === v x) &&&
        (lookupo x gamma typ));
      Fresh.three (fun m n t ->
        (expr === app m n) &&&
        (infero gamma m (arr t typ)) &&&
        (infero gamma n t));
      Fresh.four (fun x l t t' ->
        (expr === abs x l) &&&
        (typ  === arr t t') &&&
        (infero ((inj_pair x t)%gamma) l t'))
    ]
  in
  infero (nil()) expr typ

let show_string = fun x -> x
let show_fstring : (string, string) fancy -> string = show_fancy show_string
let show_fpair f g = show_fancy (fun (a,b) -> sprintf "(%s,%s)" (f a) (g b))

let show_env xs =
  MiniKanren.List.show (show_fpair show_fstring show_ftyp) xs

let varX : (string,string) fancy = inj@@lift "x"
let varY = inj@@lift "y"
let varF = inj@@lift "f"

let inj_list_p xs = inj_list @@ List.map (fun (x,y) -> inj_pair x y) xs

let _noFreeVars =
  run_exn show_lam    1 q (REPR (fun q -> lookupo varX (inj_list []) q                                     )) qh;
  run_exn show_lam    1 q (REPR (fun q -> lookupo varX (inj_list_p [(varX, v varX)]) q                     )) qh;
  run_exn show_lam    1 q (REPR (fun q -> lookupo varX (inj_list_p [(varY, v varY); (varX, v varX)]) q     )) qh;

  run_exn show_string 1 q (REPR (fun q -> lookupo q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varX)   )) qh;
  run_exn show_string 1 q (REPR (fun q -> lookupo q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varY)   )) qh;
  run_exn show_typ    1 q (REPR (fun q -> infero (abs varX (app (v varX) (v varX)))                q)) qh;
  ()

let runT n  = runR ltyp_of_ftyp show_typ show_ltyp n

let runL n  = runR llam_of_flam show_lam show_llam n
let _withFreeVars =
  (*run  show_env    1 q (REPR (fun q -> lookupo varX q (v varY)                                            )) qh; *)
  runT     1 q (REPR (fun q -> infero (abs varX (v varX)) q                )) qh;
  runT     1 q (REPR (fun q -> infero (abs varF (abs varX (app (v varF) (v varX)))) q        )) qh;
  runT     1 q (REPR (fun q -> infero (abs varX (abs varF (app (v varF) (v varX)))) q        )) qh;
  runL     1 q (REPR (fun q -> infero q (arr (p varX) (p varX))                              )) qh;
  ()

(* ************************************** **)

(* reifier for lambdas  *)
(* let lam_of_flam isVar f =
  let cond : (_,_) fancy -> bool = fun x -> isVar !!!x in
  let rec helper (t: flam) : llam =
    if cond t then refine_fancy2 t isVar
    else match coerce_fancy t with
    | V v when cond s -> MetaVar (index_of_var v)
    | V v
      Value (if cond s then V (refine_fancy2 s isVar) else V (Value (coerce_fancy s)))
    | App (f,g) ->
      Value (App ((if cond f then refine_fancy2 f isVar else helper f),
                  (if cond g then refine_fancy2 g isVar else helper g)))
    | Abs (n, body) ->
      Value (App ( Value (if cond n then V (refine_fancy2 n isVar) else V (Value (coerce_fancy n))) ,
                   (if cond body then refine_fancy2 body isVar else helper body)
                 )
            )
  in
  helper f *)
