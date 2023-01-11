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

(* module Pprintast_ = Pprintast *)
open Ppxlib
open Stdppx

let str_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.empty (fun ~loc:_ ~path:_ (_, info) ->
    List.concat_map info ~f:Reify_impl.process1)
;;

let () =
  Deriving.add "reify" ~str_type_decl ~extension:(fun ~loc ~path:_ ->
    Reify_impl.reifier_of_core_type ~loc Reify)
  |> Deriving.ignore
;;

let () =
  Deriving.add "prj_exn" ~extension:(fun ~loc ~path:_ ->
    Reify_impl.reifier_of_core_type ~loc Prj_exn)
  |> Deriving.ignore
;;
