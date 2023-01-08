(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2023
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

val run
  :  Warnings.loc
  -> Parsetree.type_declaration
  -> Parsetree.type_declaration * Parsetree.type_declaration
