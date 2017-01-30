(* Type inferencer for STLC *)
open Printf
open MiniKanren
open Tester

type 'a f = ('a, 'a) fancy
type ('varname, 'self) glam =
  | V of 'varname
  | App of 'self * 'self
  | Abs of 'varname * 'self

type lam = (string, lam) glam
type flam  = ((string f, flam) glam, lam) fancy
type flam2 =  (string f, (flam, lam) fancy) glam

type llam  = ((string logic, llam) glam) logic
type llam2 =  (string logic, llam logic) glam

let stringl_of_stringf (c: var_checker) (x: string f): string logic =
  let rec helper x : string logic =
    if c#isVar x
    then refine_fancy x c helper
    else Value (coerce_fancy x)
  in
  helper x

let llam_of_flam (c: var_checker) f =
  let rec helper (t: flam) : llam =
    if c#isVar t then refine_fancy t c helper
    else match coerce_fancy t with
    | V s when c#isVar s -> Value (V (stringl_of_stringf c s))
    | V s                -> Value (V (Value (coerce_fancy s)) )
    | App (f,g)          -> Value (App (helper f, helper g))
    | Abs (n, body)      -> Value (Abs (stringl_of_stringf c n, helper body))
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

(************************************************************************************************************)
@type ('a, 'b) gtyp =
  | P of 'a                  (* primitive *)
  | Arr of 'b * 'b           (* arrow *)
  with gmap;;

type typ  = (string, typ) gtyp
type ftyp = ((string f, ftyp) gtyp, typ) fancy
type ltyp = (string logic, ltyp) gtyp logic

module TypFamilies = FMapALike0(struct
  type t = (string f, ftyp) gtyp
  type r = typ
end)
let (_: ((string f, ftyp) gtyp, (string f, ftyp) gtyp) fancy ->
        ((string f, ftyp) gtyp,                   typ) fancy)
  = TypFamilies.wrap

let p s     : ftyp = TypFamilies.wrap @@ inj @@ lift @@ P s
let arr x y : ftyp = TypFamilies.wrap @@ inj @@ lift @@ Arr (x,y)

(* reifier for types *)
let ltyp_of_ftyp (c: var_checker) f =
  let rec helper (t: ftyp) : ltyp =
    if c#isVar t then refine_fancy t c helper
    else match coerce_fancy t with
    | P s when c#isVar s -> Value (P (refine_fancy s c (stringl_of_stringf c) ))
    | P s                -> Value (P (Value (coerce_fancy s)) )
    | Arr (f,g) -> Value (Arr (helper f, helper g))
  in
  helper f

let (_: var_checker -> ftyp -> ltyp) = ltyp_of_ftyp

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
