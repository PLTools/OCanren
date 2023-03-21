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
module List = Stdlib.List

@type 'a logic'                = 'a logic                                   with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic' = logic;;

@type ('a, 'b) t = 'a * 'b with show, gmap, html, eq, compare, foldl, foldr, fmt
let fmap f g x = GT.gmap(t) f g x;;

@type ('a, 'b) ground          = 'a * 'b                                    with show, gmap, html, eq, compare, foldl, foldr, fmt
@type ('a, 'b) logic           = ('a * 'b) logic'                           with show, gmap, html, eq, compare, foldl, foldr, fmt
@type ('a, 'b) pair            = ('a, 'b) ground                            with show, gmap, html, eq, compare, foldl, foldr, fmt
@type ('a, 'b) pair_logic      = ('a, 'b) logic                             with show, gmap, html, eq, compare, foldl, foldr, fmt

type ('a, 'b) groundi = ('a * 'b) ilogic

type ('a, 'b) injected = ('a, 'b) groundi

let logic = {
  logic with
  GT.plugins =
    object(this)
      method compare       = logic.GT.plugins#compare
      method gmap          = logic.GT.plugins#gmap
      method eq            = logic.GT.plugins#eq
      method foldl         = logic.GT.plugins#foldl
      method foldr         = logic.GT.plugins#foldr
      method html          = logic.GT.plugins#html
      method fmt           = logic.GT.plugins#fmt
      method show    fa fb = GT.show(logic') (fun l -> GT.show(ground) fa fb l)
    end
}

let inj f g p = to_logic (GT.gmap(ground) f g p)

let pair x y = Logic.inj (x, y)

let reify : 'a 'b 'c 'd . ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t ->
  (('a,'c) groundi, ('b, 'd) logic) Reifier.t =
  fun ra rb ->
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun self ->
      Reifier.compose Reifier.reify
       ( ra >>= fun fa ->
         rb >>= fun fb ->
          let rec foo = function
              | Var (v, xs) ->
                Var (v, Stdlib.List.map foo xs)
              | Value x -> Value (GT.gmap t fa fb x)
          in
          Env.Monad.return foo
        ))

let prj_exn : 'a 'b 'c 'd . ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t ->
  (('a, 'c) groundi, ('b, 'd) ground) Reifier.t =
  fun ra rb ->
    let ( >>= ) = Env.Monad.bind in
    Reifier.compose Reifier.prj_exn
    (ra >>= fun fa ->
     rb >>= fun fb ->
     Env.Monad.return (fun x -> GT.gmap t fa fb x))

let reify_pair = reify
let prj_exn_pair = prj_exn
