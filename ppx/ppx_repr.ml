(*
 * OCanren. PPX suntax extensions.
 * Copyright (C) 2015-2019
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
    print_endline "expression";
    match e with
    | { pexp_desc=Pexp_construct ({txt=Lident "REPR";_}, Some e); _} as expr ->

      let text = string_of_expression e in
      { expr with pexp_desc =
                    Pexp_tuple [Ast_helper.Exp.constant (Pconst_string (text,None)); e] }
    | e -> super#expression e
end
(*
let string_constants_of = object
  inherit [string list] Ast_traverse.fold as super
  method! expression e acc =
    let acc = super#expression e acc in
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, _)) -> s :: acc
    | _ -> acc
end
let string_constants_of_structure = string_constants_of#structure
let map_constants_of = object
  inherit Ast_traverse.map as super
  method! expression e =
    let e2 = super#expression e in
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, x)) -> {e with pexp_desc = Pexp_constant (Pconst_string (s^s, x))}
    | _ -> e2
end
*)

let register () =
  Ppxlib.Driver.register_transformation
    ~impl:(fun s  ->
              (* let strings = string_constants_of_structure s [] in
              print_endline @@ Base.String.concat ~sep:"," strings;
              let s = map_constants_of#structure s in *)
              mapper#structure s)
    "repr"
