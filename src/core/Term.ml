(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
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

open Printf

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

type t = Obj.t

module Var =
  struct
    type term   = t
    type env    = int
    type scope  = int
    type anchor = int ref

    let tabling_env = -1

    let unused_index = -1

    let non_local_scope = -6

    let new_scope =
      let scope = ref 0 in
      fun () -> (incr scope; !scope)

    let global_anchor = ref (-8)

    type t =
      { anchor        : anchor;
        env           : env;
        index         : int;
        mutable subst : term option;
        scope         : scope;
        constraints   : term list
      }

    let make ~env ~scope index = {
      env         = env;
      anchor      = global_anchor;
      subst       = None;
      constraints = [];
      index;
      scope;
    }

    let dummy =
      let env   = 0 in
      let scope = 0 in
      make ~env ~scope 0

    let var_tag, var_size =
      let dummy = Obj.repr dummy in
      Obj.tag dummy, Obj.size dummy

    let is_valid_anchor anchor = anchor == global_anchor

    let has_var_structure tx sx x =
      if tx = var_tag && sx = var_size then
        let anchor = (Obj.obj x).anchor in
        (Obj.is_block @@ Obj.repr anchor) && is_valid_anchor anchor
      else false

    let reify r { index ; constraints } = index, List.map (fun x -> r @@ Obj.obj x) constraints

    let equal x y = x == y || x.index = y.index && x.env = y.env

    let compare x y =
      if x.index <> y.index then x.index - y.index else x.env - y.env

    let hash x = Hashtbl.hash (x.env, x.index)

    let describe ppf { index } = Format.fprintf ppf "_.%d" index

    let pp ppt ppf x = match x.constraints with
    | [] -> describe ppf x
    | cs ->
      let open Format in
      let ppcs = pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") ppt in
      fprintf ppf "%a{=/= %a}" describe x ppcs cs
  end

module VarSet = Set.Make(Var)
module VarTbl = Hashtbl.Make(Var)

module VarMap =
  struct
    include Map.Make(Var)

    let iteri f m =
      let i = ref 0 in
      iter (fun k v -> f !i k v; incr i) m
  end

type value = Obj.t

let repr = Obj.repr
let obj = Obj.obj

let is_box t =
  t <= Obj.last_non_constant_constructor_tag &&
  t >= Obj.first_non_constant_constructor_tag

let is_int = (=) Obj.int_tag
let is_str = (=) Obj.string_tag
let is_dbl = (=) Obj.double_tag

let is_val t = is_int t || is_str t || is_dbl t

let check_val t =
  if not @@ is_val t then invalid_arg @@ sprintf "OCanren fatal: invalid value tag (%d)" t

let var x =
  let x = Obj.repr x in
  let tx = Obj.tag x in
  if is_box tx then
    let sx = Obj.size x in
    if Var.has_var_structure tx sx x then Some (obj x) else None
  else None

let pp =
  let open Format in
  let rec helper ppf x =
    let tx = Obj.tag x in
    if is_box tx
    then (
      let sx = Obj.size x in
      if Var.has_var_structure tx sx x
      then (
        let v = obj x in
        match v.Var.constraints with
        | [] -> Var.describe ppf v
        | cs ->
          fprintf
            ppf
            "%a{=/= %a}"
            Var.describe
            v
            (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") helper)
            cs)
      else (
        let rec inner i : unit =
          if i < sx
          then (
            if i > 0 then fprintf ppf ", ";
            helper ppf (Obj.field x i);
            inner (i + 1))
        in
        fprintf ppf "boxed %d <" tx;
        inner 0;
        fprintf ppf ">"))
    else (
      check_val tx;
      if tx = Obj.int_tag
      then fprintf ppf "int<%d>" @@ obj x
      else if tx = Obj.string_tag
      then fprintf ppf "string<%s>" @@ obj x
      else if tx = Obj.double_tag
      then fprintf ppf "double<%e>" @@ obj x
      else failwith "Dynamic pretty printing of some special tags is not supported")
  in
  helper

let show x = Format.asprintf "%a" pp x

let rec map ~fvar ~fval x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if Var.has_var_structure tx sx x then
      fvar @@ obj x
    else
      let y = Obj.dup x in
      for i = 0 to sx - 1 do
        Obj.set_field y i @@ map ~fvar ~fval (Obj.field x i)
      done;
      y
  else begin
    check_val tx;
    fval x
  end

let rec iter ~fvar ~fval x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if Var.has_var_structure tx sx x then
      fvar @@ obj x
    else
      for i = 0 to sx - 1 do
        iter ~fvar ~fval (Obj.field x i)
      done;
  else begin
    check_val tx;
    fval x
  end

let rec fold ~fvar ~fval ~init x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if Var.has_var_structure tx sx x then
      fvar init @@ obj x
    else
      let rec inner i acc =
        if i < sx then
          let acc = fold ~fvar ~fval ~init:acc (Obj.field x i) in
          inner (i+1) acc
        else acc
      in
      inner 0 init
  else begin
    check_val tx;
    fval init x
  end

exception Different_shape of int * int

type label = L | R

let rec fold2 ~fvar ~fval ~fk ~init x y =
  let tx, ty = Obj.tag x, Obj.tag y in
  match is_box tx, is_box ty with
  | true, true -> begin
    let sx, sy = Obj.size x, Obj.size y in
    match Var.has_var_structure tx sx x, Var.has_var_structure ty sy y with
    | true, true    -> fvar init (obj x) (obj y)
    | true, false   -> fk init L (obj x) y
    | false, true   -> fk init R (obj y) x
    | false, false  ->
      if (tx = ty) && (sx = sy) then
        let fx, fy = Obj.field x, Obj.field y in
        let rec inner i acc =
          if i < sx then
            let acc = fold2 ~fvar ~fval ~fk ~init:acc (fx i) (fy i) in
            inner (i+1) acc
          else acc
        in
        inner 0 init
      else raise (Different_shape (tx, ty))
    end
  | true, false ->
    check_val ty;
    let sx = Obj.size x in
    if Var.has_var_structure tx sx x then fk init L (obj x) y else raise (Different_shape (tx, ty))
  | false, true ->
    check_val tx;
    let sy = Obj.size y in
    if Var.has_var_structure ty sy y then fk init R (obj y) x else raise (Different_shape (tx, ty))
  | false, false ->
    check_val tx;
    check_val ty;
    if tx = ty then
      fval init x y
    else raise (Different_shape (tx, ty))

let rec equal x y =
  try
    fold2 x y ~init:true
      ~fvar:(fun acc v u ->
        acc &&
        (Var.equal v u) &&
        (List.length v.Var.constraints = List.length u.Var.constraints) &&
        (List.for_all2 equal v.Var.constraints u.Var.constraints)
      )
      ~fval:(fun acc x y -> acc && (x = y))
      ~fk:(fun _ _ _ _ -> false)
  with Different_shape _ -> false

let compare' = compare

let rec compare x y =
  try
    fold2 x y ~init:0
      ~fvar:(fun acc v u ->
        if acc <> 0 then acc
        else
          let acc = Var.compare v u in
          if acc <> 0 then acc
          else List.fold_left2 (fun acc x y -> if acc <> 0 then acc else compare x y) 0 v.Var.constraints u.Var.constraints
      )
      ~fval:(fun acc x y -> if acc <> 0 then acc else (compare' x y))
      ~fk:(fun _ _ _ _ -> -1)
  with Different_shape (tx, ty) -> compare' tx ty

let rec hash x = fold x ~init:1
  ~fvar:(fun acc v -> Hashtbl.hash (Var.hash v, List.fold_left (fun acc x -> Hashtbl.hash (acc, hash x)) acc v.Var.constraints))
  ~fval:(fun acc x -> Hashtbl.hash (acc, Hashtbl.hash x))
