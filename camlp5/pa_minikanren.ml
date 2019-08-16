(*
 * pa_minikanren: a camlp5 extension to implement syntax-level
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

(** {1 Pa_minikanren --- a camlp5 syntax extension for miniKanren syntax constructs} *)

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
  
EXTEND
  GLOBAL: expr;

  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> false, (<:expr< $lid:i$ >>, <:expr< $lid:i$ >>)
      | i = UIDENT -> true , (<:expr< $uid:i$ >>, <:expr< $lid:decapitalize i$ >>)
      | i = UIDENT; "."; j = SELF ->
          let f, j = j in
          let rec loop (m1, m2) =
            function
            | <:expr< $x$ . $y$ >>, <:expr< $a$ . $z$ >> -> loop (<:expr< $m1$ . $x$ >>, <:expr< $m2$ . $a$ >>) (y, z)
            | (e, e') -> <:expr< $m1$ . $e$ >>, <:expr< $m2$ . $e'$ >> 
          in
          f, loop (<:expr< $uid:i$ >>, <:expr< $uid:i$ >>) j
    ]]
  ;
    
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
            <:expr< MiniKanren.Fresh.three (fun $pa$ $pb$ $pc$ -> $loop tl$) >>
        | a::b::tl ->
            let rez = loop tl in
            let pa = <:patt< $lid:a$ >> in
            let pb = <:patt< $lid:b$ >> in
            <:expr< MiniKanren.Fresh.two (fun $pa$ $pb$ -> $rez$) >>
        | a::[] ->
            let pa = <:patt< $lid:a$ >> in
            <:expr< MiniKanren.Fresh.one (fun $pa$ -> $body$) >>
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
    "top" RIGHTA [ l=ocanren_expr; "|"; r=ocanren_expr -> <:expr< MiniKanren.disj $l$ $r$ >> ] |
          RIGHTA [ l=ocanren_expr; "&"; r=ocanren_expr -> <:expr< MiniKanren.conj $l$ $r$ >> ] |
    [ "fresh"; vars=LIST1 LIDENT SEP ","; "in"; b=ocanren_expr LEVEL "top" ->
       List.fold_right
         (fun x b ->
            let p = <:patt< $lid:x$ >> in
            <:expr< MiniKanren.call_fresh ( fun $p$ -> $b$ ) >>
         )
         vars
         b                                        
    ] |
    "primary" [
        l=ocanren_term; "==";  r=ocanren_term -> <:expr< MiniKanren.unify $l$ $r$ >> 
      | l=ocanren_term; "=/="; r=ocanren_term -> <:expr< MiniKanren.diseq $l$ $r$ >>
      | l=ocanren_term                        -> l
      | "("; e=ocanren_expr LEVEL "top"; ")" -> e                                            
    ]
  ];
  
  ocanren_term:  [
    "top" LEFTA [l=ocanren_term; r=ocanren_term -> <:expr< $l$ $r$ >> ] |
    [ c=expr_ident ->
      let uid, (o, _) = c in
      if uid
      then <:expr< MiniKanren.inj (MiniKanren.lift $o$) >>
      else o ] |
    [ c=expr_ident; "("; ts=LIST1 ocanren_term SEP ","; ")" ->
      let uid, (o, d) = c in
      let c = if uid then d else o in
      List.fold_left (fun e x -> <:expr< $e$ $x$ >>) c ts ] |
    [ c=INT ->
      let n = <:expr< $int:c$ >> in
      <:expr< MiniKanren.Std.nat $n$ >> ] |
    [ "("; ts=LIST0 ocanren_term SEP ","; ")" ->
      match ts with
      | []      -> <:expr< MiniKanren.inj (MiniKanren.lift ()) >>
      | [x]     -> x
      | x::y::t ->
         List.fold_left
           (fun p x -> <:expr< MiniKanren.Std.pair p x >>)
           <:expr< MiniKanren.Std.pair $x$ $y$ >>
           t
    ] |
    [ s=STRING ->
      let s = <:expr< $str:s$ >> in
      <:expr< MiniKanren.inj (MiniKanren.lift $s$) >>
    ] |
    [ "true"   -> <:expr< MiniKanren.Std.LBool.truo >> ] |
    [ "false"  -> <:expr< MiniKanren.Std.LBool.falso >> ] |
    [ "["; ts=LIST0 ocanren_term SEP ";"; "]" ->
      match ts with
      | [] -> <:expr< MiniKanren.Std.nil () >>
      | _  -> List.fold_right (fun x l -> <:expr< MiniKanren.Std.LList.cons $x$ $l$ >> ) ts <:expr< MiniKanren.Std.nil () >>
    ] |
    RIGHTA [ l=ocanren_term; "::"; r=ocanren_term -> <:expr< MiniKanren.Std.LList.cons $l$ $r$ >> ] |
    [ "("; t=ocanren_term LEVEL "top"; ")" -> t ]
  ];
  
END;
