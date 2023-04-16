(* SPDX-License-Identifier: LGPL-2.1-or-later *)
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

open Ppxlib
open Ppxlib.Ast_builder.Default

let () =
  let pattern =
    let open Ast_pattern in
    pstr (pstr_eval (pexp_ident __) drop ^:: nil)
  in
  let rec helper ~loc l =
    match l with
    | Lident s -> [%pat? Lident [%p ppat_constant ~loc (Pconst_string (s, loc, None))]]
    | Ldot (l, s) ->
        [%pat? Ldot ([%p helper ~loc l], [%p ppat_constant ~loc (Pconst_string (s, loc, None))])]
    | Lapply (_, _) -> failwith " not supported"
  in
  Ppxlib.Driver.register_transformation
    "lident"
    ~extensions:
      [ Extension.declare "lident" Extension.Context.Pattern pattern (fun ~loc ~path:_ ->
            helper ~loc)
      ]
;;
