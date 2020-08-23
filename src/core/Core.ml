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

type stat = {
    mutable unification_count : int;
    mutable unification_time  : Mtime.span;
    mutable conj_counter      : int;
    mutable disj_counter      : int;
    mutable delay_counter     : int
}

let stat = {
    unification_count = 0;
    unification_time  = Mtime.Span.zero;
    conj_counter      = 0;
    disj_counter      = 0;
    delay_counter     = 0
}

let unification_counter () = stat.unification_count
let unification_time    () = stat.unification_time
let conj_counter        () = stat.conj_counter
let disj_counter        () = stat.disj_counter
let delay_counter       () = stat.delay_counter

let unification_incr     () = stat.unification_count <- stat.unification_count + 1
let unification_time_incr t =
  stat.unification_time <- Mtime.Span.add stat.unification_time (t ())

let conj_counter_incr  () = stat.conj_counter  <- stat.conj_counter + 1
let disj_counter_incr  () = stat.disj_counter  <- stat.disj_counter + 1
let delay_counter_incr () = stat.delay_counter <- stat.delay_counter + 1

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

module Answer :
  sig
    (* [Answer.t] - a type that represents (untyped) answer to a query *)
    type t

    (* [make env t] creates the answer from the environment and term (with constrainted variables)  *)
    val make : Env.t -> Term.t -> t

    (* [lift env a] lifts the answer into different environment, replacing all variables consistently *)
    val lift : Env.t -> t -> t

    (* [env a] returns an environment of the answer *)
    val env : t -> Env.t

    (* [unctr_term a] returns a term with unconstrained variables *)
    val unctr_term : t -> Term.t

    (* [ctr_term a] returns a term with constrained variables *)
    val ctr_term : t -> Term.t

    (* [disequality a] returns all disequality constraints on variables in term as a list of bindings *)
    val disequality : t -> Subst.Binding.t list

    (* [equal t t'] syntactic equivalence (not an alpha-equivalence) *)
    val equal : t -> t -> bool

    (* [hash t] hashing that is consistent with syntactic equivalence *)
    val hash : t -> int
  end = struct
    type t = Env.t * Term.t

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
                let acc = Subst.(Binding.({var; term}))::acc in
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
              let new_var = Env.fresh ~scope:Term.Var.non_local_scope env' in
              Term.VarTbl.add vartbl v new_var;
              {new_var with Term.Var.constraints =
                List.map (fun x -> helper x) v.Term.Var.constraints
                |> List.sort Term.compare
              }
          )
      in
      (env', helper t)

    let check_envs_exn env env' =
      if Env.equal env env' then () else
        failwith "OCanren fatal (Answer.check_envs): answers from different environments"

    let equal (env, t) (env', t') =
      check_envs_exn env env';
      Term.equal t t'

    let hash (env, t) = Term.hash t
  end

module Prunes : sig
  type rez = Violated | NonViolated
  type ('a, 'b) reifier = Env.t -> ('a, 'b) Logic.injected -> 'b
  type 'b cond = 'b -> bool
  type t

  val empty   : t
  (* Returns false when constraints are violated *)
  val recheck : t -> Env.t -> Subst.t -> rez
  val check_one : t -> Env.t -> Subst.t -> Term.VarTbl.key -> rez
  val extend  : t -> Term.VarTbl.key -> ('a, 'b) reifier -> 'b cond -> t
end = struct
  type rez = Violated | NonViolated
  type ('a, 'b) reifier = Env.t -> ('a, 'b) Logic.injected -> 'b
  type reifier_untyped = Env.t -> Obj.t -> Obj.t
  type 'b cond = 'b -> bool
  type cond_untyped = Obj.t -> bool

  type t = (reifier_untyped * cond_untyped) Term.VarMap.t

  let empty = Term.VarMap.empty

  let check_one map env subst term =
    try
      let (reifier, cond) = Term.VarMap.find term map in
      let reified = reifier env (Obj.magic @@ Subst.apply env subst term) in
      if cond reified
      then NonViolated
      else Violated
    with Not_found -> NonViolated

  exception Fail
  let recheck ps env s =
    try
      Term.VarMap.iter (fun k (reifier, checker) ->
          let reified = reifier env (Obj.magic @@ Subst.apply env s k) in
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
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Disequality.t
      ; prunes: Prunes.t
      ; scope : Term.Var.scope
      }

    type reified = Env.t * Term.t

    let empty () =
      { env   = Env.empty ()
      ; subst = Subst.empty
      ; ctrs  = Disequality.empty
      ; prunes = Prunes.empty
      ; scope = Term.Var.new_scope ()
      }

    let env   {env} = env
    let subst {subst} = subst
    let constraints {ctrs} = ctrs
    let scope {scope} = scope
    let prunes {prunes} = prunes

    let fresh {env; scope} = Env.fresh ~scope env

    let new_scope st = {st with scope = Term.Var.new_scope ()}

    let unify x y ({env; subst; ctrs; scope} as st) =
        match Subst.unify ~scope env subst x y with
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

    (* returns always non-empty list *)
    let reify x {env; subst; ctrs} =
      let answ = Subst.reify env subst x in
      match Disequality.reify env subst ctrs x with
      | [] -> [Answer.make env answ]
      | diseqs ->
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
                      match Env.var env dt with
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

type goal = State.t Stream.t goal'

let success st = Stream.single st
let failure _  = Stream.nil

let (===) x y st =
  unification_incr ();
  let t = Timer.make () in
  match State.unify x y st with
  | Some st -> unification_time_incr t; success st
  | None    -> unification_time_incr t; failure st

let unify = (===)

let (=/=) x y st =
  match State.diseq x y st with
  | Some st -> success st
  | None    -> failure st

let diseq = (=/=)

let delay g st =
  delay_counter_incr ();
  Stream.from_fun (fun () -> g () st)

let conj f g st =
  conj_counter_incr ();
  Stream.bind (f st) g

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

let disj_base f g st = Stream.mplus (f st) (Stream.from_fun (fun () -> g st))

let disj f g st =
  disj_counter_incr ();
  let st = State.new_scope st in
  disj_base f g |> (fun g -> Stream.from_fun (fun () -> g st))

let (|||) = disj

let (?|) gs st =
  let st = State.new_scope st in
  let rec inner = function
  | [g]   -> g
  | g::gs -> disj_base g (inner gs)
  | [] -> failwith "Wrong argument of (?!)"
  in
  inner gs |> (fun g -> Stream.from_fun (fun () -> g st))

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
      let q = Env.fresh ~scope env in
      let r = Env.fresh ~scope env in
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
  Stream.bind stream (fun st -> Stream.of_list @@ State.reify args st)
  |> Stream.map (fun answ ->
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
              Stream.suspend ~is_ready @@ fun () -> helper !cache !cache seen
            else
              (* consume one answer term from cache and `lift` it to the current environment *)
              let answ, tail = (Answer.lift env @@ List.hd curr), List.tl curr in
              match State.unify (Obj.repr args) (Answer.unctr_term answ) st with
                | None -> helper start tail seen
                | Some ({subst=subst'; ctrs=ctrs'} as st') ->
                  begin
                  (* check `answ` disequalities against external substitution *)
                  let ctrs = ListLabels.fold_left (Answer.disequality answ) ~init:Disequality.empty
                    ~f:(let open Subst.Binding in fun acc {var; term} ->
                      match Disequality.add env Subst.empty acc (Term.repr var) term with
                      (* we should not violate disequalities *)
                      | None     -> assert false
                      | Some acc -> acc
                    )
                  in
                  match Disequality.recheck env subst' ctrs (Subst.split subst') with
                  | None      -> helper start tail seen
                  | Some ctrs ->
                    let st' = {st' with ctrs = Disequality.merge_disjoint env subst' ctrs' ctrs} in
                    Stream.(cons st' (from_fun @@ fun () -> helper start tail seen))
                  end
          in
          helper !cache !cache []

      end

    type t = Cache.t H.t

    let make_answ args st =
      match State.reify args st with
      | [answ] ->
          let env = Env.create ~anchor:Term.Var.tabling_env in
          Answer.lift env answ
      | _      -> failwith "should not happen"

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
            match Disequality.recheck env subst' ctrs (Subst.split subst') with
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
