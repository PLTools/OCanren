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

open Logic

module Answer :
  sig
    (* [Answer.t] - a type that represents (untyped) answer to a query *)
    type t

    (* [make env t] creates the answer from the environment and term (with constrainted variables)  *)
    val make : VarEnv.t -> Term.t -> t

    (* [lift env a] lifts the answer into different environment, replacing all variables consistently *)
    val lift : VarEnv.t -> t -> t

    (* [env a] returns an environment of the answer *)
    val env : t -> VarEnv.t

    (* [unctr_term a] returns a term with unconstrained variables *)
    val unctr_term : t -> Term.t

    (* [ctr_term a] returns a term with constrained variables *)
    val ctr_term : t -> Term.t

    (* [disequality a] returns all disequality constraints on variables in term as a list of bindings *)
    val disequality : t -> VarSubst.Binding.t list

    (* [equal t t'] syntactic equivalence (not an alpha-equivalence) *)
    val equal : t -> t -> bool

    (* [hash t] hashing that is consistent with syntactic equivalence *)
    val hash : t -> int
  end = struct
    type t = VarEnv.t * Term.t

    let make env t = (env, t)

    let env (env, _) = env

    let unctr_term (_, t) =
      Term.map t
        ~fval:(fun x -> Term.repr x)
        ~fvar:(fun v -> Term.repr {v with Term.Var.constraints = []})

    let ctr_term (_, t) = t

    let disequality (env, t) =
      let rec helper acc x =
        Term.fold x ~init:acc
          ~fval:(fun acc _ -> acc)
          ~fvar:(fun acc var ->
            ListLabels.fold_left var.Term.Var.constraints ~init:acc
              ~f:(fun acc ctr_term ->
                let ctr_term = Term.repr ctr_term in
                let var = {var with Term.Var.constraints = []} in
                let term = unctr_term @@ (env, ctr_term) in
                let acc = VarSubst.(Binding.({var; term}))::acc in
                helper acc ctr_term
              )
          )
      in
      helper [] t

    let lift env' (env, t) =
      let vartbl = Term.VarTbl.create 31 in
      let rec helper x =
        Term.map x
          ~fval:(fun x -> Term.repr x)
          ~fvar:(fun v -> Term.repr @@
            try
              Term.VarTbl.find vartbl v
            with Not_found ->
              let new_var = VarEnv.fresh ~scope:Term.Var.non_local_scope env' in
              Term.VarTbl.add vartbl v new_var;
              {new_var with Term.Var.constraints =
                List.map (fun x -> helper x) v.Term.Var.constraints
                |> List.sort Term.compare
              }
          )
      in
      (env', helper t)

    let check_envs_exn env env' =
      if VarEnv.equal env env' then () else
        failwith "OCanren fatal (Answer.check_envs): answers from different environments"

    let equal (env, t) (env', t') =
      check_envs_exn env env';
      Term.equal t t'

    let hash (env, t) = Term.hash t
  end

module Prunes : sig
  type rez = Violated | NonViolated
  type ('a, 'b) reifier = VarEnv.t -> ('a, 'b) Logic.injected -> 'b
  type 'b cond = 'b -> bool
  type t

  val empty   : t
  (* Returns false when constraints are violated *)
  val recheck : t -> VarEnv.t -> VarSubst.t -> rez
  val check_one : t -> VarEnv.t -> VarSubst.t -> Term.VarTbl.key -> rez
  val extend  : t -> Term.VarTbl.key -> ('a, 'b) reifier -> 'b cond -> t
end = struct
  type rez = Violated | NonViolated
  type ('a, 'b) reifier = VarEnv.t -> ('a, 'b) Logic.injected -> 'b
  type reifier_untyped = VarEnv.t -> Obj.t -> Obj.t
  type 'b cond = 'b -> bool
  type cond_untyped = Obj.t -> bool

  type t = (reifier_untyped * cond_untyped) Term.VarMap.t

  let empty = Term.VarMap.empty

  let check_one map env subst term =
    try
      let (reifier, cond) = Term.VarMap.find term map in
      let reified = reifier env (Obj.magic @@ VarSubst.apply env subst term) in
      if cond reified
      then NonViolated
      else Violated
    with Not_found -> NonViolated

  exception Fail
  let recheck ps env s =
    try
      Term.VarMap.iter (fun k (reifier, checker) ->
          let reified = reifier env (Obj.magic @@ VarSubst.apply env s k) in
          if not (checker reified)
          then raise Fail
       ) ps;
       NonViolated
    with Fail -> Violated

  let extend map var rr cond =
    Term.VarMap.add (Obj.magic var) (Obj.magic (rr,cond)) map

end

module State =
  struct
    type t =
      { env   : VarEnv.t
      ; subst : VarSubst.t
      ; ctrs  : Disequality.t
      ; prunes: Prunes.t
      ; scope : Term.Var.scope
      }

    type reified = VarEnv.t * Term.t

    let empty () =
      { env   = VarEnv.empty ()
      ; subst = VarSubst.empty
      ; ctrs  = Disequality.empty
      ; prunes = Prunes.empty
      ; scope = Term.Var.new_scope ()
      }

    let env   {env} = env
    let subst {subst} = subst
    let constraints {ctrs} = ctrs
    let scope {scope} = scope
    let prunes {prunes} = prunes

    let fresh {env; scope} = VarEnv.fresh ~scope env

    let new_scope st = {st with scope = Term.Var.new_scope ()}

    let unify x y ({env; subst; ctrs; scope} as st) =
        match VarSubst.unify ~scope env subst x y with
        | None -> None
        | Some (prefix, subst) ->
          match Disequality.recheck env subst ctrs prefix with
          | None      -> None
          | Some ctrs ->
            match Prunes.recheck (prunes st) env subst with
            | Prunes.Violated -> None
            | NonViolated -> Some {st with subst; ctrs}

    let diseq x y ({env; subst; ctrs; scope} as st) =
      match Disequality.add env subst ctrs x y with
      | None      -> None
      | Some ctrs -> Some {st with ctrs}

    let reify x {env; subst; ctrs} =
      let answ = VarSubst.reify env subst x in
      let diseqs = Disequality.reify env subst ctrs x in
      if List.length diseqs = 0 then
        [Answer.make env answ]
      else
        ListLabels.map diseqs ~f:(fun diseq ->
          let rec helper forbidden t =
            Term.map t
              ~fval:(fun x -> Term.repr x)
              ~fvar:(fun v -> Term.repr @@
                if List.mem v.Term.Var.index forbidden then v
                else
                  {v with Term.Var.constraints =
                    Disequality.Answer.extract diseq v
                    |> List.filter (fun dt ->
                      match VarEnv.var env dt with
                      | Some u  -> not (List.mem u.Term.Var.index forbidden)
                      | None    -> true
                    )
                    |> List.map (fun x -> helper (v.Term.Var.index::forbidden) x)
                    (* TODO: represent [Var.constraints] as [Set];
                     * TODO: hide all manipulations on [Var.t] inside [Var] module;
                     *)
                    |> List.sort Term.compare
                  }
              )
          in
          Answer.make env (helper [] answ)
      )
  end

let (!!!) = Obj.magic

type 'a goal' = State.t -> 'a

type goal = State.t RStream.t goal'

let success st = RStream.single st
let failure _  = RStream.nil

let (===) x y st =
  match State.unify x y st with
  | Some st -> success st
  | None    -> failure st

let unify = (===)
          
let (=/=) x y st =
  match State.diseq x y st with
  | Some st -> success st
  | None    -> failure st

let diseq = (=/=)
          
let delay g st = RStream.from_fun (fun () -> g () st)

let conj f g st = RStream.bind (f st) g

let structural var rr k st =
  match Term.var var with
  | None -> success st
  | Some v ->
      let new_constraints = Prunes.extend (State.prunes st) v rr k in
      match Prunes.check_one new_constraints (State.env st) (State.subst st) v with
      | Prunes.Violated -> failure st
      | NonViolated -> success { st with State.prunes = new_constraints }

let (&&&) = conj
let (?&) gs = List.fold_right (&&&) gs success

let disj_base f g st = RStream.mplus (f st) (RStream.from_fun (fun () -> g st))

let disj f g st = let st = State.new_scope st in disj_base f g |> (fun g -> RStream.from_fun (fun () -> g st))

let (|||) = disj

let (?|) gs st =
  let st = State.new_scope st in
  let rec inner = function
  | [g]   -> g
  | g::gs -> disj_base g (inner gs)
  | [] -> failwith "Wrong argument of (?!)"
  in
  inner gs |> (fun g -> RStream.from_fun (fun () -> g st))

let conde = (?|)

let call_fresh f st =
  let x = State.fresh st in
  f x st

module Fresh =
  struct
    let succ prev f = call_fresh (fun x -> prev (f x))

    let zero  f = f
    let one   f = succ zero f
    let two   f = succ one f

    (* N.B. Manual inlining of numerals will speed-up OCanren a bit (mainly because of less memory consumption) *)
    (* let two   g = fun st ->
      let scope = State.scope st in
      let env = State.env st in
      let q = VarEnv.fresh ~scope env in
      let r = VarEnv.fresh ~scope env in
      g q r st *)

    let three f = succ two f
    let four  f = succ three f
    let five  f = succ four f

    let q     = one
    let qr    = two
    let qrs   = three
    let qrst  = four
    let pqrst = five
  end

(* ******************************************************************************* *)
(* ************************** Reification stuff ********************************** *)

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module Curry =
  struct
    let one = (@@)
    let succ k f x = k (fun tup -> f (x, tup))
  end

module Uncurry =
  struct
    let one = (@@)
    let succ k f (x,y) = k (f x) y
  end

module LogicAdder :
  sig
    val zero : goal -> goal
    val succ : ('a -> State.t -> 'd) -> (('e, 'f) injected -> 'a) -> State.t -> ('e, 'f) injected * 'd
  end = struct
    let zero f      = f
    let succ prev f = call_fresh (fun logic st -> (logic, prev (f logic) st))
  end

module ReifyTuple = struct
  let one x env = make_rr env x
  let succ prev (x, xs) env = (make_rr env x, prev xs env)
end

let succ n () =
  let adder, app, ext, uncurr = n () in
  (LogicAdder.succ adder, ReifyTuple.succ app, ExtractDeepest.succ ext, Uncurry.succ uncurr)

let one   () = (LogicAdder.(succ zero)), ReifyTuple.one, ExtractDeepest.ext2, Uncurry.one
let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let qrstu = five

let run n g h =
  let adder, reifier, ext, uncurr = n () in
  let args, stream = ext @@ adder g @@ State.empty () in
  RStream.bind stream (fun st -> RStream.of_list @@ State.reify args st)
  |> RStream.map (fun answ ->
    uncurr h @@ reifier (Obj.magic @@ Answer.ctr_term answ) (Answer.env answ)
  )

(** ************************************************************************* *)
(** Tabling primitives                                                        *)

module Table :
  sig
    (* Type of table.
     * Table is a map from answer term to the set of answer terms,
     * i.e. Answer.t -> [Answer.t]
     *)
    type t

    val create   : unit -> t

    val call : t -> ('a -> goal) -> 'a -> goal
  end = struct

    module H = Hashtbl.Make(Answer)

    module Cache :
      sig
        type t

        val create    : unit -> t

        val add       : t -> Answer.t -> unit
        val contains  : t -> Answer.t -> bool
        val consume   : t -> 'a -> goal
      end =
      struct
        (* Cache is a pair of queue-like list of answers plus hashtbl of answers;
         * Queue is used because new answers may arrive during the search,
         * we store this new answers to the end of the queue while read from the beginning.
         * Hashtbl is used for a quick check that new added answer is not already contained in the cache.
         *)
        type t = Answer.t list ref * unit H.t

        let create () = (ref [], H.create 11)

        let add (cache, tbl) answ =
          cache := List.cons answ !cache;
          H.add tbl answ ()

        let contains (_, tbl) answ =
          try
            H.find tbl answ;
            true
          with Not_found -> false

        let consume (cache, _) args =
          let open State in fun {env; subst; scope} as st ->
          let st = State.new_scope st in
          (* [helper start curr seen] consumes answer terms from cache one by one
           *   until [curr] (i.e. current pointer into cache list) is not equal to [seen]
           *   (i.e. to the head of seen part of the cache list)
           *)
          let rec helper start curr seen =
            if curr == seen then
              (* update `seen` - pointer to already seen part of cache *)
              let seen = start in
              (* delayed check that current head of cache is not equal to head of seen part *)
              let is_ready () = seen != !cache  in
              (* delayed thunk starts to consume unseen part of cache  *)
              RStream.suspend ~is_ready @@ fun () -> helper !cache !cache seen
            else
              (* consume one answer term from cache and `lift` it to the current environment *)
              let answ, tail = (Answer.lift env @@ List.hd curr), List.tl curr in
              match State.unify (Obj.repr args) (Answer.unctr_term answ) st with
                | None -> helper start tail seen
                | Some ({subst=subst'; ctrs=ctrs'} as st') ->
                  begin
                  (* check `answ` disequalities against external substitution *)
                  let ctrs = ListLabels.fold_left (Answer.disequality answ) ~init:Disequality.empty
                    ~f:(let open VarSubst.Binding in fun acc {var; term} ->
                      match Disequality.add env VarSubst.empty acc (Term.repr var) term with
                      (* we should not violate disequalities *)
                      | None     -> assert false
                      | Some acc -> acc
                    )
                  in
                  match Disequality.recheck env subst' ctrs (VarSubst.split subst') with
                  | None      -> helper start tail seen
                  | Some ctrs ->
                    let st' = {st' with ctrs = Disequality.merge_disjoint env subst' ctrs' ctrs} in
                    RStream.(cons st' (from_fun @@ fun () -> helper start tail seen))
                  end
          in
          helper !cache !cache []

      end

    type t = Cache.t H.t

    let make_answ args st =
      let env = VarEnv.create ~anchor:Term.Var.tabling_env in
      let [answ] = State.reify args st in
      Answer.lift env answ

    let create () = H.create 1031

    let call tbl g args = let open State in fun ({env; subst; ctrs} as st) ->
      (* we abstract away disequality constraints before lookup in the table *)
      let abs_st = {st with ctrs = Disequality.empty} in
      let key = make_answ args abs_st in
      try
        (* slave call *)
        Cache.consume (H.find tbl key) args st
      with Not_found ->
        (* master call *)
        let cache = Cache.create () in
        H.add tbl key cache;
        (* auxiliary goal for addition of new answer to the cache  *)
        let hook ({env=env'; subst=subst'; ctrs=ctrs'} as st') =
          let answ = make_answ args st' in
          if not (Cache.contains cache answ) then begin
            Cache.add cache answ;
            (* TODO: we only need to check diff, i.e. [subst' \ subst] *)
            match Disequality.recheck env subst' ctrs (VarSubst.split subst') with
            | None      -> failure ()
            | Some ctrs ->
              success {st' with ctrs = Disequality.merge_disjoint env subst' ctrs ctrs'}
          end
          else failure ()
        in
        ((g args) &&& hook) abs_st
  end

module Tabling =
  struct
    let succ n () =
      let currier, uncurrier = n () in
      let sc = (Curry.succ : (('a -> 'b) -> 'c) -> ((((_, _) injected as 'k) * 'a -> 'b) -> 'k -> 'c)) in
      (sc currier, Uncurry.succ uncurrier)

    let one () = ((Curry.(one) : ((_, _) injected -> _) as 'x -> 'x), Uncurry.one)

    let two   () = succ one ()
    let three () = succ two ()
    let four  () = succ three ()
    let five  () = succ four ()

    let tabled n g =
      let tbl = Table.create () in
      let currier, uncurrier = n () in
      currier (Table.call tbl @@ uncurrier g)

    let tabledrec n g_norec =
      let tbl = Table.create () in
      let currier, uncurrier = n () in
      let g = ref (fun _ -> assert false) in
      let g_rec args = uncurrier (g_norec !g) args in
      let g_tabled = Table.call tbl g_rec in
      g := currier g_tabled;
      !g
  end

let id x = x
