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

IFDEF STATS THEN
type stat = {mutable walk_count : int}

let stat = {walk_count = 0}

let walk_counter () = stat.walk_count
let walk_incr () = stat.walk_count <- stat.walk_count + 1
END

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

module Binding =
  struct

    type t =
      { var   : Term.Var.t
      ; term  : Term.t
      }

    let equal {var=v; term=t} {var=u; term=p} =
      (Term.Var.equal v u) || (Term.equal t p)

    let compare {var=v; term=t} {var=u; term=p} =
      let res = Term.Var.compare v u in
      if res <> 0 then res else Term.compare t p

    let hash {var; term} = Hashtbl.hash (Term.Var.hash var, Term.hash term)

    let pp ppf {var; term} =
      Format.fprintf ppf "{ var.idx = %d; term=%a }" var.Term.Var.index Term.pp term
  end

let varmap_of_bindings : Binding.t list -> Term.t Term.VarMap.t =
  Stdlib.List.fold_left (fun (acc: _ Term.VarMap.t) Binding.{var;term} ->
    assert (not (Term.VarMap.mem var acc));
    Term.VarMap.add var term acc
  )
  Term.VarMap.empty

type t = Term.t Term.VarMap.t

let empty = Term.VarMap.empty

let of_map m = m

let split s = Term.VarMap.fold (fun var term xs -> Binding.{ var ; term }::xs) s []

let pp ppf s =
  let open Format in
  fprintf ppf "{subst| " ;
  Term.VarMap.iter (fun x t -> fprintf ppf "%a |- %a; " Term.pp (Term.repr x) Term.pp t) s ;
  fprintf ppf "|subst}"

type lterm = Var of Term.Var.t | Value of Term.t

let walk env subst =

  (* walk var *)
  let rec walkv v =
    let () = IFDEF STATS THEN walk_incr () ELSE () END in
    Env.check_exn env v ;

    match v.Term.Var.subst with
    | Some term -> walkt term
    | None ->
        try walkt (Term.VarMap.find v subst)
        with Not_found -> Var v

  (* walk term *)
  and walkt t =
    let () = IFDEF STATS THEN walk_incr () ELSE () END in

    match Env.var env t with
    | Some v -> walkv v
    | None   -> Value t
  in

  walkv

(* same as [Term.map] but performs [walk] on the road *)
let map ~fvar ~fval env subst x =
  let rec deepfvar v =
    Env.check_exn env v;
    match walk env subst v with
    | Var v   -> fvar v
    | Value x -> Term.map x ~fval ~fvar:deepfvar
  in
  Term.map x ~fval ~fvar:deepfvar

(* same as [Term.iter] but performs [walk] on the road *)
let iter ~fvar ~fval env subst x =
  let rec deepfvar v =
    Env.check_exn env v;
    match walk env subst v with
    | Var v   -> fvar v
    | Value x -> Term.iter x ~fval ~fvar:deepfvar
  in
  Term.iter x ~fval ~fvar:deepfvar

exception Occurs_check

let rec occurs env subst var term = iter env subst term ~fval:(fun _ -> ())
  ~fvar:(fun v -> if Term.Var.equal v var then raise Occurs_check)

(* [var] must be free in [subst], [term] must not be the same variable *)
let extend ~scope env subst var term =
  if Runconf.do_occurs_check () then occurs env subst var term ;

  (* It is safe to modify variables destructively if the case of scopes match.
   * There are two cases:
   * 1) If we do unification just after a conde, then the scope is already incremented and nothing goes into
   *    the fresh variables.
   * 2) If we do unification after a fresh, then in case of failure it doesn't matter if
   *    the variable is be distructively substituted: we will not look on it in future.
   *)
  if scope = var.Term.Var.scope && scope <> Term.Var.non_local_scope then begin
    var.subst <- Some term ;
    subst
  end else
    Term.VarMap.add var term subst

exception Unification_failed

let unify ?(subsume=false) ?(scope=Term.Var.non_local_scope) env subst x y =
  (* The idea is to do the unification and collect the unification prefix during the process *)
  let extend var term (prefix, subst) =
    let subst = extend ~scope env subst var term in
    Binding.{ var ; term }::prefix, subst
  in

  let rec helper x y acc = Term.fold2 x y ~init:acc
    ~fvar:begin fun ((_, subst) as acc) x y ->
      match walk env subst x, walk env subst y with
      | Var x, Var y ->
        if Term.Var.equal x y then acc
        else
          let x, y = if not subsume && Term.Var.compare x y < 0 then y, x else x, y in
          extend x (Term.repr y) acc
      | Var x, Value y -> extend x y acc
      | Value x, Var y -> extend y x acc
      | Value x, Value y  -> helper x y acc
    end
    ~fval:begin fun acc x y ->
      if x = y then acc
      else raise Unification_failed
    end
    ~fk:begin fun ((_, subst) as acc) l v y ->
      if subsume && l = Term.R
      then raise Unification_failed
      else match walk env subst v with
      | Var v   -> extend v y acc
      | Value x -> helper x y acc
    end
  in

  try
    let x, y = Term.(repr x, repr y) in
    Some (helper x y ([], subst))
  with Term.Different_shape _ | Unification_failed | Occurs_check -> None

let unify_map env subst map =
  let vars, terms = Term.VarMap.fold (fun v t (vs, ts) -> Term.repr v :: vs, t::ts) map ([], []) in
  unify env subst vars terms

let merge_disjoint env = Term.VarMap.union @@ fun _ _ ->
  invalid_arg "OCanren fatal (Subst.merge_disjoint): substitutions intersect"

let subsumed env subst = Term.VarMap.for_all @@ fun var term ->
  match unify env subst var term with
  | Some ([], _) -> true
  | _            -> false

let apply env subst x = Obj.magic @@ map env subst (Term.repr x) ~fvar:Term.repr ~fval:Term.repr

let freevars env subst x = Env.freevars env @@ apply env subst x

module Answer =
  struct

    type t = Term.t

    let subsumed env x y =
      match unify ~subsume:true env empty y x with
      | Some _ -> true
      | None   -> false
  end

let reify = apply
