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

type stat = {mutable walk_count : int}

let stat = {walk_count = 0}

let walk_counter () = stat.walk_count
let walk_incr () = stat.walk_count <- stat.walk_count + 1
(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

module Binding =
  struct
    type t =
      { var   : Term.Var.t
      ; term  : Term.t
      }

    let is_relevant env vs {var; term} =
      (Term.VarSet.mem var vs) ||
      (match Env.var env term with Some v -> Term.VarSet.mem v vs | None -> false)

    let equal {var=v; term=t} {var=u; term=p} =
      (Term.Var.equal v u) || (Term.equal t p)

    let compare {var=v; term=t} {var=u; term=p} =
      let res = Term.Var.compare v u in
      if res <> 0 then res else Term.compare t p

    let hash {var; term} = Hashtbl.hash (Term.Var.hash var, Term.hash term)
  end

type t = Term.t Term.VarMap.t

let empty = Term.VarMap.empty

let of_list =
  ListLabels.fold_left ~init:empty ~f:(let open Binding in fun subst {var; term} ->
    if not @@ Term.VarMap.mem var subst then
      Term.VarMap.add var term subst
    else
      invalid_arg "OCanren fatal (Subst.of_list): invalid substituion"
  )

let of_map m = m

let split s = Term.VarMap.fold (fun var term xs -> Binding.({var; term})::xs) s []

type lterm = Var of Term.Var.t | Value of Term.t

let walk env subst x =
  (* walk var *)
  let rec walkv env subst v =
    walk_incr ();
    Env.check_exn env v;
    match v.Term.Var.subst with
    | Some term -> walkt env subst (Obj.magic term)
    | None ->
        try walkt env subst (Term.VarMap.find v subst)
        with Not_found -> Var v
  (* walk term *)
  and walkt env subst t =
    walk_incr ();
    match Env.var env t with
    | Some v -> walkv env subst v
    | None   -> Value t
  in
  walkv env subst x

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

(* same as [Term.fold] but performs [walk] on the road *)
let fold ~fvar ~fval ~init env subst x =
  let rec deepfvar acc v =
    Env.check_exn env v;
    match walk env subst v with
    | Var v   -> fvar acc v
    | Value x -> Term.fold x ~fval ~fvar:deepfvar ~init:acc
  in
  Term.fold x ~init ~fval ~fvar:deepfvar

exception Occurs_check

let rec occurs env subst var term =
  iter env subst term
    ~fvar:(fun v -> if Term.Var.equal v var then raise Occurs_check)
    ~fval:(fun x -> ())

let extend ~scope env subst var term  =
  (* if occurs env subst var term then raise Occurs_check *)
  if Runconf.do_occurs_check () then occurs env subst var term;
    (* assert (VarEnv.var env var <> VarEnv.var env term); *)
  occurs env subst var term;

  (* It is safe to modify variables destructively if the case of scopes match.
   * There are two cases:
   * 1) If we do unification just after a conde, then the scope is already incremented and nothing goes into
   *    the fresh variables.
   * 2) If we do unification after a fresh, then in case of failure it doesn't matter if
   *    the variable is be distructively substituted: we will not look on it in future.
   *)
  if (scope = var.Term.Var.scope) && (scope <> Term.Var.non_local_scope)
  then begin
    var.subst <- Some (Obj.repr term);
    subst
  end
    else
      Term.VarMap.add var (Term.repr term) subst

exception Unification_failed

let unify ?(subsume=false) ?(scope=Term.Var.non_local_scope) env subst x y =
  (* The idea is to do the unification and collect the unification prefix during the process *)
  let extend var term (prefix, subst) =
    let subst = extend ~scope env subst var term in
    (Binding.({var; term})::prefix, subst)
  in
  let rec helper x y acc =
    let open Term in
    fold2 x y ~init:acc
      ~fvar:(fun ((_, subst) as acc) x y ->
        match walk env subst x, walk env subst y with
        | Var x, Var y      ->
          if Var.equal x y then acc else extend x (Term.repr y) acc
        | Var x, Value y    -> extend x y acc
        | Value x, Var y    -> extend y x acc
        | Value x, Value y  -> helper x y acc
      )
      ~fval:(fun acc x y ->
          if x = y then acc else raise Unification_failed
      )
      ~fk:(fun ((_, subst) as acc) l v y ->
          if subsume && (l = Term.R)
          then raise Unification_failed
          else match walk env subst v with
          | Var v    -> extend v y acc
          | Value x  -> helper x y acc
      )
  in
  try
    let x, y = Term.(repr x, repr y) in
    Some (helper x y ([], subst))
  with Term.Different_shape _ | Unification_failed | Occurs_check -> None

let apply env subst x = Obj.magic @@
  map env subst (Term.repr x)
    ~fvar:(fun v -> Term.repr v)
    ~fval:(fun x -> Term.repr x)

let freevars env subst x =
  Env.freevars env @@ apply env subst x

let is_bound = Term.VarMap.mem

let merge env subst1 subst2 = Term.VarMap.fold (fun var term -> function
  | Some s  -> begin
    match unify env s (Obj.magic var) term with
    | Some (_, s') -> Some s'
    | None         -> None
    end
  | None    -> None
) subst1 (Some subst2)

let merge_disjoint env =
  Term.VarMap.union (fun _ _ ->
    invalid_arg "OCanren fatal (Subst.merge_disjoint): substitutions intersect"
  )

let subsumed env subst =
  Term.VarMap.for_all (fun var term ->
    match unify env subst (Obj.magic var) term with
    | Some ([], _)  -> true
    | _             -> false
  )

module Answer =
  struct
    type t = Term.t

    let subsumed env x y =
      match unify ~subsume:true env empty y x with
      | Some _ -> true
      | None   -> false
  end

let reify env subst x =
  map env subst (Term.repr x)
    ~fvar:(fun v -> Term.repr v)
    ~fval:(fun x -> Term.repr x)
