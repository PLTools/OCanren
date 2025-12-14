(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2025
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University
 *)

val run :
     Warnings.loc
  -> Ppxlib.type_declaration list
  -> Ppxlib.type_declaration list * Ppxlib.type_declaration list
