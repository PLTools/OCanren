(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2015-2025
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

(*
   # type 'a t = 'a OCanren.Std.List.injected [@@deriving reify];;
type 'a t = 'a OCanren.Std.List.injected
val reify :
  ('a, 'b) OCanren__.Logic.Reifier.t ->
  ('a OCanren.Std.List.injected, 'b OCanren.Std.List.logic)
  OCanren__.Logic.Reifier.t = <fun>
val prj_exn :
  ('a, 'b) OCanren__.Logic.Reifier.t ->
  ('a OCanren.Std.List.injected, 'b OCanren.Std.List.ground)
  OCanren__.Logic.Reifier.t = <fun>

# [%reify: GT.int OCanren.Std.List.injected];;
- : ('_weak2 OCanren.ilogic OCanren.Std.List.injected,
     '_weak2 OCanren.logic OCanren.Std.List.logic)
    OCanren__.Logic.Reifier.t
= <fun>
*)
open Ppxlib
open Stdppx

let str_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.empty (fun ~loc:_ ~path:_ (_, info) ->
      List.concat_map info ~f:Reify_impl.process_str)
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
