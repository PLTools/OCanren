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

let rec fold f = function
| [h]  -> h
| h::t -> f h (fold f t)
;;

EXTEND
  GLOBAL: expr;

  (* TODO: support conde expansion here *)
  expr: LEVEL "expr1" [
    [ "fresh"; "("; vars=LIST0 LIDENT; ")"; clauses=LIST1 expr LEVEL "." ->
      let listed_clauses = List.fold_right
        (fun x acc -> <:expr< (List.cons $x$ $acc$) >>)
        clauses <:expr< [] >>
      in
      let __herr0 = String.concat " " vars in

      let body = <:expr< bind_star $listed_clauses$ >> in
      let body = <:expr<
(*        let () = Printf.printf "create inc in fresh ==== (%s)\n%!"
                    $str:__herr0$ in *)
        delay (fun () ->
      (*    let () = Printf.printf "inc in fresh forced: (%s)\n%!"
                    $str:__herr0$ in *)
          $body$) >>
      in
      let ans =
        List.fold_right (fun x e ->
          let p = <:patt< $lid:x$ >> in
          <:expr< call_fresh (fun $p$ -> $e$) >>
        ) (List.rev vars) body
      in
      (* let rec ans = <:expr<
        let () = Printf.printf "create inc in fresh ====== (%s)\n%!"
          $str:__herr0$ in
        delay (fun () -> $body$) (* it is inc *)
      >>

      in *)
      ans
    ] |
    [ "defer"; subj=expr LEVEL "." ->
      <:expr< delay (fun () -> $subj$) >>
    ]
  ];

END;
