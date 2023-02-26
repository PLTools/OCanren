(*
 * OCanren.
 * Copyright (C) 2015-2023
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

module Answer =
  struct
    module S = Set.Make(Term)

    (* answer is a conjunction of single disequalities, i.g. (x =/= 1 /\ y =/= 2);
     * in order to efficiently extract disequalities relevant to particular variable we use map
     *)
    type t = S.t Term.VarMap.t

    let empty = Term.VarMap.empty

    let add env t var term =
      try
        let terms = S.add term @@ Term.VarMap.find var t in
        Term.VarMap.add var terms @@ Term.VarMap.remove var t
      with Not_found ->
        Term.VarMap.add var (S.singleton term) t

    let mem env t var term =
      try
        S.mem term @@ Term.VarMap.find var t
      with Not_found -> false

    let extract t v =
      try S.elements @@ Term.VarMap.find v t with Not_found -> []

    let subsumed env t t' =
      (* we should check that for each binding from [t'] there is
       * a binding in [t] that subsumes it;
       * Examples:
       *    (x =/= _.0) <= (x =/= 1 /\ x =/= 2), but
       *    (x =/= _.0) and (x =/= 1 /\ y =/= 2) are not ordered
       *)
      Term.VarMap.for_all (fun var terms' ->
        try
          let terms = Term.VarMap.find var t in
          S.for_all (fun term' ->
            S.exists (fun term ->
              Subst.Answer.subsumed env term term'
            ) terms
          ) terms'
        with Not_found -> false
      ) t'
  end

exception Disequality_violated
exception Disequality_fulfilled

(* Disequality constraints are represented as formula in CNF
* where each atom is single disequality
* (i.g. ({x =/= t} \/ {y =/= u}) /\ ({y =/= v} \/ {z =/= w}))
*
* Optimisation:
* For each disjunct in the formula we choose one `sample` (i.e single disequality {x =/= t}).
* Whenever we want to check the whole disequality constraint we
* can check single `sample` from each conjunct.
* If `sample` check is passed (i.e {x =/= t} holds in current substitution) we can
* skip checks of other disjuncts in this conjunct.
* Also note that after unification of two terms we can
* check only those disequalities that involves changed variables.
* Because of that we maintain an index - a map from variable index to
* list of conjuncts for which this variable is a `sample`.
* When `sample` check fails, we change index.
* We choose another `sample` {y =/= u} and add it to the map for variable {y}.
* There is no need to check previous samples in the future (because its assumption is already broken in current substitution)
*)

module Disjunct :
  sig
    (* Disjunction.t is a set of single disequalities joint by disjunction *)
    type t

    (* [make env subst x y] creates new disjunct from the disequality [x =/= y] *)
    val make : Env.t -> Subst.t -> 'a -> 'a -> t

    (* [sample disj] returns an index of variable involved in some disequality inside disjunction *)
    val samplevar : t -> Term.Var.t

    (* [recheck env subst disj] - checks that disjunction of disequalities is
     *   not violated in (current) substitution.
     *   This function is designed to incrementally refine disequalities
     *   with a series of more and more specialized substitutions.
     *   If arbitary substitutions are passed the result may be invalid.
     *)
    val recheck : Env.t -> Subst.t -> t -> t

    val is_relevant : Env.t -> Subst.t -> t -> Term.VarSet.t -> bool

    val freevars : Env.t -> Subst.t -> t -> Term.VarSet.t

    val subsumed : Env.t -> Subst.t -> t -> t -> bool

    val simplify : Env.t -> Subst.t -> t -> t option

    val reify : Env.t -> Subst.t -> t -> Subst.Binding.t list
  end =
  struct
    type t = Term.t Term.VarMap.t

    let update :t -> _ -> _ = fun init ->
      ListLabels.fold_left ~init
        ~f:(fun acc {Subst.Binding.var; term} ->
          if Term.VarMap.mem var acc then
            (* in this case we have subformula of the form (x =/= t1) \/ (x =/= t2) which is always SAT *)
            raise Disequality_fulfilled
          else
            Term.VarMap.add var term acc
        )

    let of_list = update Term.VarMap.empty

    let samplevar t = fst @@ Term.VarMap.max_binding t

    type status =
      | Fulfiled
      | Violated
      | Refined of Subst.Binding.t list

    let refine env subst x y =
      match Subst.unify env subst x y with
      | None              -> Fulfiled
      | Some (prefix, _)  ->
        let cond = fun {Subst.Binding.var; term} ->
          if Term.Var.is_wildcard var || (Env.is_wc env term)
            then false else true
        in
        match Stdlib.List.filter cond prefix with
        | [] -> Violated
        | prefix -> Refined prefix

    let make env subst x y =
      match refine env subst x y with
      | Refined delta -> of_list delta
      | Fulfiled      -> raise Disequality_fulfilled
      | Violated      -> raise Disequality_violated

    let rec recheck : Env.t -> Subst.t ->  t -> t = fun env subst t ->
      let var, term = Term.VarMap.max_binding t in
      let unchecked = Term.VarMap.remove var t in
      match refine env subst (Obj.magic var) term with
      | Fulfiled       -> raise Disequality_fulfilled
      | Refined delta  -> update unchecked delta
      | Violated       ->
        if Term.VarMap.is_empty unchecked then
          raise Disequality_violated
        else
          recheck env subst unchecked

    let simplify env subst : t -> t option = fun ds ->
      try
        let result = Term.VarMap.fold (fun var term acc ->
          match refine env subst (Obj.magic var) term with
          | Fulfiled       -> raise Disequality_fulfilled
          | Violated       -> acc
          | Refined delta  -> delta @ acc
        ) ds []
        in
        (* We should not get empty substituion delta here,
         * because it would mean that disequality is violated.
         * But we had to detect violations during search via `check`.
         *)
        assert (match result with [] -> false | _ -> true);
        Some (of_list result)
      with Disequality_fulfilled -> None

    let reify env subst t =
      Term.VarMap.fold (fun var term xs -> Subst.(Binding.({var; term})::xs)) t []

    let is_relevant env subst t fv =
      (* left those disjuncts that contains binding only for variables from [fv],
       * otherwise it's possible to pick binding (x =/= t) from disjunct for
       * variable [x] that is not in [fv],
       * assign [t'] ([t =/= t']) to [x] and thus fulfill the disequality
       *)
       Term.VarMap.for_all (fun var term ->
         (Term.VarSet.mem var fv) ||
         (match Env.var env term with Some u -> Term.VarSet.mem u fv | None -> false)
       ) t

    let freevars env subst t =
      Term.VarMap.fold (fun _ term acc ->
        Term.VarSet.union acc @@ Subst.freevars env subst term
      ) t Term.VarSet.empty

    let subsumed env subst t t' =
      Subst.(subsumed env (of_map t') (of_map t))

  end

module Conjunct :
  sig
    type t

    val empty : t

    val is_empty : t -> bool

    val make : Env.t -> Subst.t -> 'a -> 'a -> t

    val split : t -> t Term.VarMap.t

    val recheck : Env.t -> Subst.t -> t -> t

    val project : Env.t -> Subst.t -> t -> Term.VarSet.t -> t

    val merge_disjoint : Env.t -> Subst.t -> t -> t -> t

    (* [diff env subst t' t] computes diff of two conjuncts, that is [t' \ t].
     *   Returns a pair where first element is conjunct
     *   of constraints that were changed (i.e. refined) in [t'],
     *   and the second element is conjunct of entirely new constraints in [t'],
     *   that absent in [t].
     *)
    val diff : Env.t -> Subst.t -> t -> t -> t * t

    val reify : Env.t -> Subst.t -> t -> 'a -> Answer.t list
  end = struct
    let next_id = ref 0

    module M = Map.Make(struct type t = int let compare = (-) end)

    type t = Disjunct.t M.t

    let empty = M.empty

    let is_empty = M.is_empty

    let make env subst x y =
      let id = !next_id in
      next_id := !next_id + 1;
      M.singleton id (Disjunct.make env subst x y)

    let split : t -> t Term.VarMap.t =
      (* TODO(Kakadu): rewriter *)
      let group_by : ('a -> Term.Var.t) -> 'a M.t -> 'a M.t Term.VarMap.t
        = fun (* *) extract mapa ->
          M.fold (fun k v acc ->
            let new_key = extract v in
            let upd = function
              | Some old -> Some (M.add k v old)
              | None      -> Some (M.singleton k v)
            in
            Term.VarMap.update new_key upd acc
            ) mapa Term.VarMap.empty
      in
      group_by Disjunct.samplevar


    let recheck env subst t =
      M.fold (fun id disj acc ->
          try
            M.add id (Disjunct.recheck env subst disj) acc
          with Disequality_fulfilled -> acc
      ) t M.empty

    let merge_disjoint env subst =
      M.union (fun _ _ _ ->
        invalid_arg "OCanren fatal (Conjunct.merge_disjoint): conjuncts intersect"
      )

    let diff env subst t' t =
      M.fold (fun id disj' (refined, added) ->
        try
          let disj = M.find id t in
          (* refined constraint should be more specialized (i.e. subsumed by earlier constraint) *)
          assert (Disjunct.subsumed env subst disj' disj);
          (M.add id disj' refined, added)
        with Not_found ->
          (refined, M.add id disj' added)
      ) t' (M.empty, M.empty)

    let remove_subsumed env subst cs =
      M.fold (fun id disj acc ->
        if M.exists (fun _ disj' -> Disjunct.subsumed env subst disj' disj) acc then
          (* if new disjunct subsumes some another then we don't add it;
           * that's because we have conjunction of disjuncts and we can keep only
           * the most specialized disjuncts
           *)
          acc
        else
          (* otherwise we should remove all disjuncts that subsume the newly added disjunct *)
          let acc = M.filter (fun _ disj' -> not (Disjunct.subsumed env subst disj disj')) acc in
          M.add id disj acc
      ) cs M.empty

    let project env subst t fv =
      let rec helper fv =
        let fv', t' = M.fold (fun id disj (fv', conj) ->
          (* left those disjuncts that contain bindings only for variables from [fv],
           * and obtain a set of free variables from terms mentioned in those disjuncts
           *)
          if Disjunct.is_relevant env subst disj fv then
            let fv' = Term.VarSet.union fv' @@ Disjunct.freevars env subst disj in
            fv', M.add id disj conj
          else
            fv', conj
        ) t (fv, M.empty)
        in
        if Term.VarSet.equal fv fv' then t' else helper fv'
      in
      remove_subsumed env subst @@ helper fv

    let reify env subst t x =
      let t = M.fold (fun id disj acc ->
        match Disjunct.simplify env subst disj with
        | Some disj -> M.add id disj acc
        | None      -> acc
      ) t M.empty
      in
      let fv = Subst.freevars env subst x in
      let t = project env subst t fv in
      (* here we convert disequality in CNF form into DNF form;
       * we maintain a list of answers, that is a mapping [var -> term list] ---
       * list of disequality terms (without duplicates) for each variable
       *)
      M.fold (fun _ disj acc ->
        let bs = Disjunct.reify env subst disj in
          (* for each answer we append every atom in disjunct to it,
           * obtaining a list of new `extended` answers;
           * then we `concat` these lists into single list
           *)
        ListLabels.map acc ~f:(fun answ ->
            let open Subst.Binding in
            (* it might be the case that some atom in the disjunct
             * is a duplicate of some other disequality in the answer;
             * in this case we can throw away the whole disjunct (and keep only original answer)
             * because it would not produce new extended answers;
             * i.g. answer is [(x =/= 1) /\ (y =/= 2)] and the disjunct is [(x =/= 1) \/ (z =/= 3)],
             * then extended answers are [(x =/= 1) /\ (y =/= 2)] and [(x =/= 1) /\ (y =/= 2) /\ (z =/= 3)],
             * but the second one is subsumed by the first one and can be thrown away
             *)
            if Stdlib.List.exists (fun {var; term} -> Answer.mem env answ var term) bs then
              [answ]
            else
              Stdlib.List.map (fun {var; term} -> Answer.add env answ var term) bs
          ) |> Stdlib.List.concat
      ) t [Answer.empty]

  end

type t = Conjunct.t Term.VarMap.t

let empty = Term.VarMap.empty

(* merges all conjuncts (linked to different variables) into one *)
let combine env subst cstore =
  Term.VarMap.fold (fun _ -> Conjunct.merge_disjoint env subst) cstore Conjunct.empty

let merge_disjoint env subst = Term.VarMap.union (fun _ c1 c2 ->
  let c = Conjunct.merge_disjoint env subst c1 c2 in
  if Conjunct.is_empty c then None else Some c
)

let update env subst conj = merge_disjoint env subst (Conjunct.split conj)

let add env subst cstore x y =
  try
    Some (update env subst (Conjunct.make env subst x y) cstore)
  with
    | Disequality_fulfilled -> Some cstore
    | Disequality_violated -> None

let recheck env subst cstore bs =
  let helper var cstore =
    try
      let conj = Term.VarMap.find var cstore in
      let cstore = Term.VarMap.remove var cstore in
      update env subst (Conjunct.recheck env subst conj) cstore
    with Not_found -> cstore
  in
  try
    let cstore = ListLabels.fold_left bs ~init:cstore
      ~f:(let open Subst.Binding in fun cstore {var; term} ->
        let cstore = helper var cstore in
        match Env.var env term with
        | Some u -> helper u cstore
        | None   -> cstore
      )
    in
    Some cstore
  with Disequality_violated -> None

let project env subst cstore fv =
  Conjunct.(split @@ project env subst (combine env subst cstore) fv)

let reify env subst cstore x =
  Conjunct.reify env subst (combine env subst cstore) x
