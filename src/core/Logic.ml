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

open Printf

(* to avoid clash with Std.List (i.e. logic list)
  It may be fixed in next release of GT
*)
module List = Stdlib.List

@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr, fmt

let logic = {logic with
  plugins =
    object(self)
      method gmap      = logic.plugins#gmap
      method html      = logic.plugins#html
      method eq        = logic.plugins#eq
      method compare   = logic.plugins#compare
      method foldl     = logic.plugins#foldl
      method foldr     = logic.plugins#foldr
      method fmt fa fmt l = Format.fprintf fmt "%s" (self#show (Format.asprintf "%a" fa) l)
      method show fa x =
        GT.transform(logic)
          (fun fself -> object
             inherit ['a, _] @logic[show]  (GT.lift fa) fself
             method! c_Var _ s i cs =
               let c = match cs with
               | [] -> ""
               | _  -> sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ fself () l) cs)
               in
               sprintf "_.%d%s" i c
             method! c_Value _ _ x = fa x
           end)
          ()
          x
    end
}

exception Not_a_value

let to_logic x = Value x

let from_logic = function
| Value x    -> x
| Var (n, _) -> raise Not_a_value

type 'a ilogic

external inji : 'a -> 'a ilogic = "%identity"
let inj = inji

let (!!) = inj

module Reifier = struct
  type ('a, 'b) t = ('a -> 'b) Env.Monad.t

  let rec reify : ('a ilogic, 'a logic) t =
    fun env t ->
      match Term.var t with
      | None -> Value (Obj.magic t)
      | Some v ->
        let i, cs = Term.Var.reify (reify env) v in
        Var (i, cs)

  (* can be implemented more efficiently,
    * without allocation of `'a logic`,
    * but for demonstration purposes this implementation is okay
    *)
  let prj_exn : ('a ilogic, 'a) t =
    fun env t ->
      match Term.var t with
      | None -> Obj.magic t
      | Some v -> raise Not_a_value

  let prj onvar env t =
    match reify env t with
    | Value x -> x
    | Var (v, _) -> onvar v

  let apply r (env, a) = r env a

  let compose r r' env a = r' env (r env a)

  let fmap f r env a = f (r env a)

  let fcomap f r env a = r env (f a)

  let rec fix f = fun env eta -> f (fix f) env eta

  let rework :
      'a 'b.
      fv:('a Env.m -> 'b Env.m)
      -> ('a logic Env.m -> 'b logic Env.m)
      -> 'a logic Env.m
      -> 'b logic Env.m
    =
    fun ~fv fdeq x ->
      let open Env.Monad in
      let open Env.Monad.Syntax in
      let* x = x in
      match x with
      | Var (v, xs) ->
        let+ diseq = list_mapm ~f:fdeq xs in
        Var (v, diseq)
      | Value t ->
        let+ inner = fv (return t) in
        Value inner
    ;;

  let rec zed f x = f (zed f) x
end

let reify = Reifier.reify
let prj_exn = Reifier.prj_exn
(* let prj = Reifier.prj *)

class type ['a] reified = object
  method is_open : bool
  method reify   : 'b . ('a ilogic, 'b) Reifier.t -> 'b
end

let make_rr : Env.t -> 'a ilogic -> 'a reified  = fun env x ->
  object (self)
    method is_open            = Env.is_open env x
    method reify : 'b . ('a ilogic, 'b) Reifier.t -> 'b = fun reifier ->
      Reifier.apply reifier (env, x)
  end

(* let prj x = let rr = make_rr (Env.empty ()) x in rr#prj *)

(* let rec reify env x =
  match Env.var env x with
  | Some v -> let i, cs = Term.Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

let rec prjc of_int env x =
  match Env.var env x with
  | Some v -> let i, cs = Term.Var.reify (prjc of_int env) v in of_int i cs
  | None   -> Obj.magic x
 *)
