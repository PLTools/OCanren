(*
 * OCanren. PPX suntax extensions.
 * Copyright (C) 2015-2020
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

let string_of_expression e =
  Format.set_margin 1000;
  Format.set_max_indent 2000;
  let ans = Format.asprintf "%a" Pprintast.expression e in
  ans

let mapper = object
  inherit Ast_traverse.map as super

  method! expression e =
    match e with
    | { pexp_desc=Pexp_construct ({txt=Lident "REPR";_}, Some e); _} as expr ->

      let text = string_of_expression e in
      { expr with pexp_desc =
                    Pexp_tuple [Ast_helper.Exp.constant (Pconst_string (text,None)); e] }
    | e -> super#expression e
end

let () =
  Ppxlib.Driver.register_transformation
    ~impl:(fun s  -> mapper#structure s)
    "repr"
