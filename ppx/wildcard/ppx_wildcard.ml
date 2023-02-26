(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2015-2023
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
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

(** This extension performs expansion of wildcards in:

  {ul {- Unification
         {[ (q === __ % __) ]} to {[ Fresh.two (fun __1 __2 -> q === __1 % __2) ]}
      }
      {- Disequality
          {[ (q =/= Std.List.cons __ __) ]}
             to
          {[ wc (fun __1 -> wc (fun __2 -> q =/= Std.List.cons __1 __2)) ]}
      }
  }
 *)

open Ppxlib
open Ppxlib.Ast_helper

let name_of_loc loc =
  (* Format.printf "name_of_loc: %a\n%!" Location.print loc; *)
  let start = loc.Location.loc_start in
  (* let mangled_fname =
    String.map start.pos_fname ~f:(function
        | '.' -> '_'
        | c -> c)
  in *)
  let mangled_fname = start.pos_fname in
  Printf.sprintf "__%s_c%d" mangled_fname Lexing.(start.pos_cnum - start.pos_bol)
;;

let wildcard_extractor expr =
  let folder =
    object
      inherit [_] Ppxlib.Ast_traverse.fold_map as super

      method! expression e acc =
        (* Format.printf "wildcard_extractor: %a\n%!" Pprintast.expression e; *)
        let open Ppxlib.Ast_pattern in
        let loc = e.pexp_loc in
        let on_OK =
          let open Ppxlib.Ast_builder.Default in
          pexp_ident ~loc (Located.mk ~loc (Lident (name_of_loc e.pexp_loc)))
        in
        parse
          (pexp_ident (lident (string "__")))
          loc
          e
          (on_OK, e.pexp_loc :: acc)
          ~on_error:(fun () -> super#expression e acc)
    end
  in
  folder#expression expr []
;;

type kind =
  | Unif
  | Diseq

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      (* Format.printf "%a\n%!" Pprintast.expression  e; *)
      let loc = e.pexp_loc in
      let pat =
        let open Ppxlib.Ast_pattern in
        pexp_apply
          (pexp_ident (lident (string "===")))
          ((nolabel ** __) ^:: (nolabel ** __) ^:: nil)
        |> map2 ~f:(fun a b -> Unif, a, b)
        ||| (pexp_apply
               (pexp_ident (lident (string "=/=")))
               ((nolabel ** __) ^:: (nolabel ** __) ^:: nil)
            |> map2 ~f:(fun a b -> Diseq, a, b))
      in
      let on_unif (kind, l, r) =
        let l, accl = wildcard_extractor l in
        let r, accr = wildcard_extractor r in
        let f acc loc =
          let open Ppxlib.Ast_builder.Default in
          let name = name_of_loc loc in
          let name_expr = pexp_constant ~loc (Pconst_string (name, loc, None)) in
          let pat = ppat_var ~loc (Located.mk ~loc name) in
          let nameless = true in
          let make_wc, make_fresh =
            if nameless
            then [%expr wc], [%expr call_fresh]
            else [%expr named_wc [%e name_expr]], [%expr named_fresh [%e name_expr]]
          in
          match kind with
          | Diseq -> [%expr [%e make_wc] (fun [%p pat] -> [%e acc])]
          | Unif -> [%expr [%e make_fresh] (fun [%p pat] -> [%e acc])]
        in
        let init =
          match kind with
          | Unif -> [%expr [%e l] === [%e r]]
          | Diseq -> [%expr [%e l] =/= [%e r]]
        in
        let ans1 = ListLabels.fold_left ~f ~init accr in
        ListLabels.fold_left ~f ~init:ans1 accl
      in
      Ppxlib.Ast_pattern.parse pat loc e on_unif ~on_error:(fun () -> super#expression e)
  end
;;

let () = Ppxlib.Driver.register_transformation ~impl:mapper#structure "ppx_wildcard"
