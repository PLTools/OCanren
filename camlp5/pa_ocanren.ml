(*
 * pa_ocanren: a camlp5 extension to implement syntax-level
 * miniKanren constructs.
 * Copyright (C) 2015
 * Dmitri Boulytchev, St.Petersburg State University
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(** {1 Pa_ocanren --- a camlp5 syntax extension for miniKanren syntax constructs} *)

(**
  {2 General description}

  There are two syntat extensions provided: [fresh] and [defer].

  [fresh] is a direct analog to the corresponding construct in original miniKanren. It has the form
  [fresh (x y ...) g], where [x y ...] is a list of free variables, created by [fresh], [g] --- some goal.

  [defer] performs "inverse-eta-delay". It has the form [defer (g)] and expanded into [fun st -> Lazy.from_fun (fun () -> g st)].
*)

(**/**)

#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf

let rec fold_right1 f = function
| [h]  -> h
| h::t -> f h (fold_right1 f t)
;;

let rec fold_left1 f xs = List.fold_left f (List.hd xs) (List.tl xs)

let decapitalize s =
  String.init (String.length s) (function 0 -> Char.lowercase_ascii s.[0] | i -> s.[i])

let rec ctor e =
  let loc = MLast.loc_of_expr e in
  match e with
  | <:expr< $uid:u$ >>   -> Some (<:expr< $lid:decapitalize u$ >>)
  | <:expr< $m$ . $e$ >> -> (match ctor e with Some e -> Some (<:expr< $m$ . $e$ >>) | _ -> None)
  | _                    -> None

let rec fix_term e =
  let loc = MLast.loc_of_expr e in
  match e with
  | <:expr< $e1$ $e2$ >> ->
     (match ctor e1 with
      | Some e1' ->
         (match e2 with
          | <:expr< ( $list:ts$ ) >> ->
             List.fold_left (fun acc e -> <:expr< $acc$ $fix_term e$ >> ) e1' ts
          | _  -> <:expr< $e1'$ $fix_term e2$ >>
         )
      | _ ->         
         (match e with
          | <:expr< OCanren.Std.nil () >> -> e
          | _ -> <:expr< $fix_term e1$ $fix_term e2$ >>
         )
     )
  | <:expr< ( $list:ts$ ) >> ->
     (* isolater tuple case (not an arguments to a constructor *)
     (match ts with
      | [e] -> fix_term e
      | _   -> fold_right1 (fun e tup -> <:expr< OCanren.Std.pair $fix_term e$ $tup$ >> ) ts
     )
  | _ ->
    (* everything else *)
    (match ctor e with
     | Some _ -> <:expr< OCanren.inj (OCanren.lift $e$) >> (* isolated nullary constructor case *)
     | _ -> e
    )
    
EXTEND
  GLOBAL: expr;

  long_ident:
    [ RIGHTA
      [ i = LIDENT -> <:expr< $lid:i$ >>
      | i = UIDENT -> <:expr< $uid:i$ >> 
      | i = UIDENT; "."; j = SELF ->
          let rec loop m =
            function
            | <:expr< $x$ . $y$ >> -> loop <:expr< $m$ . $x$ >> y
            | e                    -> <:expr< $m$ . $e$ >>
          in
          loop <:expr< $uid:i$ >> j
    ]];

  long_ctor:
    [ RIGHTA
      [ i = UIDENT -> (<:expr< $uid:i$ >>, <:expr< $lid:decapitalize i$ >>)
      | i = UIDENT; "."; j = SELF ->
          let rec loop (m1, m2) =
            function
            | <:expr< $x$ . $y$ >>, <:expr< $a$ . $z$ >> -> loop (<:expr< $m1$ . $x$ >>, <:expr< $m2$ . $a$ >>) (y, z)
            | (e, e') -> <:expr< $m1$ . $e$ >>, <:expr< $m2$ . $e'$ >> 
          in
          loop (<:expr< $uid:i$ >>, <:expr< $uid:i$ >>) j
    ]];
    
  (* TODO: support conde expansion here *)
  expr: LEVEL "expr1" [
    [ "fresh"; "("; vars=LIST0 LIDENT; ")"; clauses=LIST1 expr LEVEL "." ->
      let body =
        let conjunctions = fold_left1
          (fun acc x -> <:expr< conj ($acc$) ($x$) >>)
          clauses
        in
        <:expr< delay (fun () -> $conjunctions$) >>
      in
      let ans =
        let rec loop = function
        | a::b::c::tl ->
            let pa = <:patt< $lid:a$ >> in
            let pb = <:patt< $lid:b$ >> in
            let pc = <:patt< $lid:c$ >> in
            <:expr< OCanren.Fresh.three (fun $pa$ $pb$ $pc$ -> $loop tl$) >>
        | a::b::tl ->
            let rez = loop tl in
            let pa = <:patt< $lid:a$ >> in
            let pb = <:patt< $lid:b$ >> in
            <:expr< OCanren.Fresh.two (fun $pa$ $pb$ -> $rez$) >>
        | a::[] ->
            let pa = <:patt< $lid:a$ >> in
            <:expr< OCanren.Fresh.one (fun $pa$ -> $body$) >>
        | []    -> body
        in
        loop vars
        (* List.fold_right (fun x e ->
          let p = <:patt< $lid:x$ >> in
          <:expr< call_fresh (fun $p$ -> $e$) >>
        ) (List.rev vars) body *)
      in
      ans
    ] |
    [ "defer"; subj=expr LEVEL "." ->
      <:expr< delay (fun () -> $subj$) >>
    ] |
    [ e=ocanren_embedding -> e ]
  ];

  ocanren_embedding: [
    [ "ocanren"; "("; e=ocanren_expr LEVEL "top"; ")" -> e ]
  ];

  ocanren_expr: [
    "top" RIGHTA [ l=ocanren_expr; "|"; r=ocanren_expr -> <:expr< OCanren.disj $l$ $r$ >> ] |
          RIGHTA [ l=ocanren_expr; "&"; r=ocanren_expr -> <:expr< OCanren.conj $l$ $r$ >> ] |
    [ "fresh"; vars=LIST1 LIDENT SEP ","; "in"; b=ocanren_expr LEVEL "top" ->
       List.fold_right
         (fun x b ->
            let p = <:patt< $lid:x$ >> in
            <:expr< OCanren.call_fresh ( fun $p$ -> $b$ ) >>
         )
         vars
         b                                        
    ] |
    "primary" [
        l=ocanren_term; "==";  r=ocanren_term -> <:expr< OCanren.unify $l$ $r$ >>
      | l=ocanren_term; "=/="; r=ocanren_term -> <:expr< OCanren.diseq $l$ $r$ >>
      | l=ocanren_term                        -> l
      | "("; e=ocanren_expr LEVEL "top"; ")" -> e                                            
    ]
  ];

  ocanren_term: [[
    t=ocanren_term' -> fix_term t
  ]];
  
  ocanren_term':  [
    "top" LEFTA [l=ocanren_term'; r=ocanren_term' -> <:expr< $l$ $r$ >> ] |
    [ c=long_ident -> c
    | c=INT ->
      let n = <:expr< $int:c$ >> in
      <:expr< OCanren.Std.nat $n$ >>
    | "("; ts=LIST0 ocanren_term' SEP ","; ")" ->
      (match ts with
       | []      -> <:expr< OCanren.inj (OCanren.lift ()) >>
       | _       -> <:expr< ( $list:ts$ ) >>
      )
    | s=STRING ->
      let s = <:expr< $str:s$ >> in
      <:expr< OCanren.inj (OCanren.lift $s$) >>
    | "true"   -> <:expr< OCanren.Std.Bool.truo >>
    | "false"  -> <:expr< OCanren.Std.Bool.falso >>
    | "["; ts=LIST0 ocanren_term' SEP ";"; "]" ->
      (match ts with
       | [] -> <:expr< OCanren.Std.nil () >>
       | _  -> List.fold_right (fun x l -> <:expr< OCanren.Std.List.cons $x$ $l$ >> ) ts <:expr< OCanren.Std.nil () >>
      )
    ] |
    RIGHTA [ l=ocanren_term'; "::"; r=ocanren_term' -> <:expr< OCanren.Std.List.cons $l$ $r$ >> ] |
    [ "("; t=ocanren_term' LEVEL "top"; ")" -> t ]
  ];
  
END;
