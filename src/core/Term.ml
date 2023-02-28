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

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

module Var =
  struct
    type env    = int
    type scope  = int
    type anchor = int list

    let tabling_env = -1

    let unused_index = -1

    let non_local_scope = -6

    let new_scope =
      let scope = ref 0 in
      fun () -> (incr scope; !scope)

    let global_anchor = [-8]

    type t =
      { anchor        : anchor;
        env           : env;
        index         : int;
        mutable subst : Obj.t option;
        scope         : scope;
        constraints   : Obj.t list
      }

    let make ~env ~scope index = {
      env         = env;
      anchor      = global_anchor;
      subst       = None;
      constraints = [];
      index;
      scope;
    }

    let is_wildcard { index } = index = -42

    let make_wc ~env ~scope = make ~env ~scope (-42)

    let dummy =
      let env   = 0 in
      let scope = 0 in
      make ~env ~scope 0

    let valid_anchor anchor =
      anchor == global_anchor

    let reify r {index; constraints} =
      (index, List.map (fun x -> r @@ Obj.obj x) constraints)

    let equal x y =
      (x.index = y.index) && (x.env = y.env)

    let compare x y =
      if x.index <> y.index then x.index - y.index else x.env - y.env

    let hash x = Hashtbl.hash (x.env, x.index)

  end

module VarSet = Set.Make(Var)
module VarTbl = Hashtbl.Make(Var)

module VarMap =
  struct
    include Map.Make(Var)

    let update k f m =
      match f (try Some (find k m) with Not_found -> None) with
      | Some x -> add k x m
      | None   -> remove k m
  end

type t = Obj.t
type value = Obj.t

let repr = Obj.repr

let var_tag, var_size =
  let dummy = Obj.repr Var.dummy in
  Obj.tag dummy, Obj.size dummy

let has_var_structure tx sx x =
  if tx = var_tag && sx = var_size
  then (
    let anchor = (Obj.obj x : Var.t).Var.anchor in
    (Obj.is_block @@ Obj.repr anchor) && Var.valid_anchor anchor)
  else false
;;

let is_box t =
  if (t <= Obj.last_non_constant_constructor_tag) &&
     (t >= Obj.first_non_constant_constructor_tag)
  then true
  else false

let is_var x =
  let x = Obj.repr x in
  let tx = Obj.tag x in
  is_box tx && has_var_structure tx (Obj.size x) x
;;

let is_int = (=) Obj.int_tag
let is_str = (=) Obj.string_tag
let is_dbl = (=) Obj.double_tag

let is_valid_tag t = (is_int t) || (is_str t) || (is_dbl t)

let is_valid_tag_exn t =
  if is_valid_tag t then () else failwith (sprintf "OCanren fatal: invalid value tag (%d)" t)

let var x =
  let x = Obj.repr x in
  let tx = Obj.tag x in
  if is_box tx then
    let sx = Obj.size x in
    if has_var_structure tx sx x then Some (Obj.magic x) else None
  else None

let rec map ~fvar ~fval x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if has_var_structure tx sx x then
      fvar @@ Obj.magic x
    else
      let y = Obj.dup x in
      for i = 0 to sx - 1 do
        Obj.set_field y i @@ map ~fvar ~fval (Obj.field x i)
      done;
      y
  else begin
    is_valid_tag_exn tx;
    fval x
  end

let rec iter ~fvar ~fval x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if has_var_structure tx sx x then
      fvar @@ Obj.magic x
    else
      for i = 0 to sx - 1 do
        iter ~fvar ~fval (Obj.field x i)
      done;
  else begin
    is_valid_tag_exn tx;
    fval x
  end

let rec show x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if has_var_structure tx sx x then
      let v = Obj.magic x in
      match v.Var.constraints with
      | [] -> Printf.sprintf "_.%d" v.Var.index
      | cs -> Printf.sprintf "_.%d{=/= %s}" v.Var.index (String.concat "; " @@ List.map show cs)
    else
      let rec inner i =
        if i < sx then
          (show @@ Obj.field x i)::(inner (i+1))
        else []
      in
      Printf.sprintf "boxed %d <%s>" tx (String.concat ", " @@ inner 0)
  else begin
    is_valid_tag_exn tx;
    if tx = Obj.int_tag then
      Printf.sprintf "int<%d>" @@ Obj.magic x
    else if tx = Obj.string_tag then
      Printf.sprintf "string<%s>" @@ Obj.magic x
    else if tx = Obj.double_tag then
      Printf.sprintf "double<%e>" @@ Obj.magic x
    else assert false
  end

let rec fold ~fvar ~fval ~init x =
  let tx = Obj.tag x in
  if (is_box tx) then
    let sx = Obj.size x in
    if has_var_structure tx sx x then
      fvar init @@ Obj.magic x
    else
      let rec inner i acc =
        if i < sx then
          let acc = fold ~fvar ~fval ~init:acc (Obj.field x i) in
          inner (i+1) acc
        else acc
      in
      inner 0 init
  else begin
    is_valid_tag_exn tx;
    fval init x
  end

exception Different_shape of int * int

type label = L | R

let rec fold2 ~fvar ~fval ~fk ~init x y =
  let tx, ty = Obj.tag x, Obj.tag y in
  match is_box tx, is_box ty with
  | true, true -> begin
    let sx, sy = Obj.size x, Obj.size y in
    match has_var_structure tx sx x, has_var_structure ty sy y with
    | true, true    -> fvar init (Obj.magic x) (Obj.magic y)
    | true, false   -> fk init L (Obj.magic x) y
    | false, true   -> fk init R (Obj.magic y) x
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
    is_valid_tag_exn ty;
    let sx = Obj.size x in
    if has_var_structure tx sx x then fk init L (Obj.magic x) y else raise (Different_shape (tx, ty))
  | false, true ->
    is_valid_tag_exn tx;
    let sy = Obj.size y in
    if has_var_structure ty sy y then fk init R (Obj.magic y) x else raise (Different_shape (tx, ty))
  | false, false ->
    is_valid_tag_exn tx;
    is_valid_tag_exn ty;
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

let describe_var ppf Var.{ index } = Format.fprintf ppf "_.%d" index

let pp =
  let open Format in
  let rec helper ppf x =
    let tx = Obj.tag x in
    if is_box tx
    then (
      let sx = Obj.size x in
      if has_var_structure tx sx x
      then (
        let v = Obj.magic x in
        match v.Var.constraints with
        | [] -> describe_var ppf v
        | cs ->
          fprintf
            ppf
            "%a{=/= %a}"
            describe_var
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
      is_valid_tag_exn tx;
      if tx = Obj.int_tag
      then fprintf ppf "int<%d>" @@ Obj.magic x
      else if tx = Obj.string_tag
      then fprintf ppf "string<%s>" @@ Obj.magic x
      else if tx = Obj.double_tag
      then fprintf ppf "double<%e>" @@ Obj.magic x
      else failwith "Dynamic pretty printing of some special tags is not supported")
  in
  fun ppf x -> helper ppf (Obj.repr x)
;;

let show x = Format.asprintf "%a" pp x
