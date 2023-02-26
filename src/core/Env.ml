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

type t = {anchor : Term.Var.env; mutable next : int}

(* TODO: document next two values *)
let last_anchor = ref 11
let first_var = 10

let empty () =
  incr last_anchor;
  {anchor = !last_anchor; next = first_var}

let create ~anchor = {anchor; next = first_var}

let fresh ~scope e =
  let v = Obj.magic (Term.Var.make ~env:e.anchor ~scope e.next) in
  e.next <- 1 + e.next;
  Obj.magic v

let wc ~scope e = Obj.magic (Term.Var.make_wc ~env:e.anchor ~scope)

let check env v = (v.Term.Var.env = env.anchor)

let check_exn env v =
  if check env v then () else failwith "OCanren fatal (Env.check): wrong environment"

let var env x =
  match Term.var x with
  | (Some v) as res -> check_exn env v; res
  | None            -> None

let is_var env x = (var env x) <> None

let is_wc env x =
  match Term.var x with
  | None -> false
  | Some v -> check_exn env v; Term.Var.is_wildcard v


let freevars env x =
  Term.fold (Term.repr x) ~init:Term.VarSet.empty
    ~fvar:(fun acc v -> Term.VarSet.add v acc)
    ~fval:(fun acc _ -> acc)

exception Open_Term

let is_open env x =
  try
    Term.iter (Term.repr x)
      ~fvar:(fun _ -> raise Open_Term)
      ~fval:(fun _ -> ());
    false
  with Open_Term -> true

let equal {anchor=a1} {anchor=a2} = (a1 = a2)

let ( <.> ) f g x = f (g x)

module Monad = struct
  type nonrec 'a t = t -> 'a

  let return a _ = a

  let fmap f r env = f (r env)

  let (<*>) f x env = f env (x env)

  let bind r k env = k (r env) env

  let chain : 'a 'b . ('a t -> 'b t) -> ('a -> 'b) t = fun f env x -> f (return x) env

  module Syntax = struct
    let (let*) x f = bind x f
    let (let+) x f = fmap f x
  end

  let (>>=) = bind

  let ( <..> ) g f =
    let open Syntax in
    return (<.>) <*> f <*> g
  ;;

  let list_mapm : f:('a t -> 'b t) -> 'a list -> 'b list t = fun ~f ->
    let rec helper = function
    | [] -> return []
    | h :: tl -> f (return h) >>= fun h ->
      helper tl >>= fun tl -> return (h::tl)
    in
    helper
end

type 'a m = 'a Monad.t
