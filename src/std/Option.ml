(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2022
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

open Logic
open Core

(* to avoid clash with Std.List (i.e. logic list) *)
(* module List = Stdlib.List *)

@type 'a logic'        = 'a logic                       with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic' = logic;;

@type 'a ground        = 'a GT.option                   with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a logic         = 'a GT.option logic'            with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a option        = 'a ground                      with show, gmap, html, eq, compare, foldl, foldr, fmt
@type 'a option_logic  = 'a logic                       with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic = {
  logic with
  GT.plugins =
    object(this)
      method compare    = logic.GT.plugins#compare
      method gmap       = logic.GT.plugins#gmap
      method eq         = logic.GT.plugins#eq
      method foldl      = logic.GT.plugins#foldl
      method foldr      = logic.GT.plugins#foldr
      method html       = logic.GT.plugins#html
      method fmt        = logic.GT.plugins#fmt
      method show    fa = GT.show(logic') (fun l -> GT.show(GT.option) fa l)
    end
};;

@type 'a t = 'a ground with show, gmap, html, eq, compare, foldl, foldr, fmt;;
let t = ground

let inj f x = to_logic (GT.(gmap option) f x)

type 'a injected = 'a ground ilogic
type 'a groundi = 'a injected

let rec reify : 'a 'b . ('a, 'b) Reifier.t -> ('a groundi, 'b logic) Reifier.t =
  fun ra ->
  let open Env.Monad.Syntax in
  let* r = Reifier.reify in
  let* fa = ra in
  Reifier.compose Reifier.reify (
    let rec foo = function
    | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
    | Value t -> Value (GT.gmap ground fa t)
    in
    Env.Monad.return foo
  )

let prj_exn : 'a 'b. ('a, 'b) Reifier.t -> ('a groundi, 'b ground) Reifier.t =
  fun ra ->
    let open Env.Monad.Syntax in
    let* r = Reifier.prj_exn in
    let* fa = ra in
    Env.Monad.return (fun x -> GT.gmap ground fa (r x))

let reify_option = reify
let prj_exn_option = prj_exn

let some x  = Logic.inji (Some x)
let none () = Logic.inji None

let option = function None -> none () | Some x -> Logic.inji (Some (x))
