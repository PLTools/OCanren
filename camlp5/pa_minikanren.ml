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

EXTEND
  GLOBAL: expr;

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
    ]
  ];

END;
