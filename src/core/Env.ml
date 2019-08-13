(*
 * OCanren.
 * Copyright (C) 2015-2017
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

let check env v = (v.Term.Var.env = env.anchor)

let check_exn env v =
  if check env v then () else failwith "OCanren fatal (Env.check): wrong environment"

let var env x =
  match Term.var x with
  | (Some v) as res -> check_exn env v; res
  | None            -> None

let is_var env x = (var env x) <> None

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