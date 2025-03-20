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

open Logic

IFDEF STATS THEN
type stat = {
  mutable unification_count : int;
  mutable unification_time  : Timer.span;
  mutable conj_counter      : int;
  mutable disj_counter      : int;
  mutable delay_counter     : int
}

let stat = {
  unification_count = 0;
  unification_time  = Timer.empty_span;
  conj_counter      = 0;
  disj_counter      = 0;
  delay_counter     = 0
}


let unification_counter () = stat.unification_count
let unification_time    () = stat.unification_time
let conj_counter        () = stat.conj_counter
let disj_counter        () = stat.disj_counter
let delay_counter       () = stat.delay_counter

let (unification_incr,unification_time_incr,conj_counter_incr,disj_counter_incr,delay_counter_incr) =
  match Sys.getenv_opt "OCANREN_BENCH_COUNTS" with
  | None -> let c = (fun _ -> ()) in (c,c,c,c,c)
  | Some _ ->
    let unification_incr     () = stat.unification_count <- stat.unification_count + 1 in
    let unification_time_incr t =
      stat.unification_time <- Timer.add_span stat.unification_time (t ()) in
    let conj_counter_incr  () = stat.conj_counter  <- stat.conj_counter + 1 in
    let disj_counter_incr  () = stat.disj_counter  <- stat.disj_counter + 1 in
    let delay_counter_incr () = stat.delay_counter <- stat.delay_counter + 1 in
    (unification_incr,unification_time_incr,conj_counter_incr,disj_counter_incr,delay_counter_incr)
END

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
  type ('a, 'b) reifier = ('a,'b) Reifier.t
  type 'b cond = 'b -> bool
  type t

  val empty   : t
  (* Returns false when constraints are violated *)
  val recheck : t -> Env.t -> Subst.t -> rez
  val check_last : t -> Env.t -> Subst.t -> rez
  val extend  : t -> Term.VarTbl.key -> ('a, 'b) reifier -> 'b cond -> t
end = struct
  type rez = Violated | NonViolated
  type ('a, 'b) reifier = ('a,'b) Reifier.t
  type reifier_untyped = Obj.t
  type 'b cond = 'b -> bool
  type cond_untyped = Obj.t -> bool

  let make_untyped : ('a, 'b) reifier -> 'b cond -> reifier_untyped * cond_untyped =
    fun a b -> Obj.magic (a,b)

  type t = (Obj.t * (reifier_untyped * cond_untyped)) list
  let empty = []

  exception Fail

  let check_last map env subst =
    try
      let (term, (reifier, checker)) = List.hd map in
      let reifier : (_,_) reifier = Obj.obj reifier in
      let reified = reifier env (Obj.magic @@ Subst.apply env subst term) in
      if not (checker reified) then raise Fail;
      NonViolated
    with Not_found -> NonViolated
       | Fail -> Violated

  let recheck (ps: t) env s =
    try
       ps |> List.iter (fun (k, (reifier, checker)) ->
          let reifier : (_,_) Reifier.t = Obj.obj reifier in
          let reified = reifier env (Obj.magic @@ Subst.apply env s k) in
          if not (checker reified) then raise Fail
       );
       NonViolated
    with Fail -> Violated

  let extend map term rr cond =
    let new_item = make_untyped rr cond in
    (Obj.repr term, new_item) :: map

end

type prines_control =
  { mutable pc_do_skip : bool
  ; mutable pc_checks_skipped : int
  ; mutable pc_max_to_skip : int
  ; mutable pc_skipped_prunes_total : int
  }

let prunes_control =
  { pc_checks_skipped = 10; pc_do_skip=false; pc_max_to_skip = 11
  ; pc_skipped_prunes_total = 0
  }

module PrunesControl = struct
  let enable_skips ~on =
(*    Printf.printf "enabling skips: %b\n%!" on;*)
    prunes_control.pc_do_skip <- on
  let is_enabled () = prunes_control.pc_do_skip
  let reset_cur_counter () = prunes_control.pc_checks_skipped <- 0
  let reset () =
    reset_cur_counter ();
    prunes_control.pc_skipped_prunes_total <- 0


  let set_max_skips n =
    assert (n>0);
    prunes_control.pc_max_to_skip <- n;
    reset ()

  let skipped_prunes () = prunes_control.pc_skipped_prunes_total
  let incr () =
    if is_enabled ()
    then
      let () = prunes_control.pc_checks_skipped <- 1 + prunes_control.pc_checks_skipped in
      prunes_control.pc_skipped_prunes_total <- 1 + prunes_control.pc_skipped_prunes_total


  let is_exceeded () =
    (not (is_enabled())) ||
    (let ans = (prunes_control.pc_checks_skipped >= prunes_control.pc_max_to_skip) in
(*    Printf.printf "is_exceeded = %b, cur_steps=%d, max_steps=%d\n%!"
      ans
      prunes_control.pc_checks_skipped
      prunes_control.pc_max_to_skip;*)
    ans
    )
end
(*
let do_skip_prunes = ref false
let prunes_checks_skipped = ref 0
let max_prunes_skipped = ref 10

let set_skip_prunes_count n =
  assert (n>0);
  max_prunes_skipped := n
*)
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
    let wc { env; scope } = Env.wc ~scope env

    let new_scope st = {st with scope = Term.Var.new_scope ()}

    let unify x y ({env; subst; ctrs; scope} as st) =
        match Subst.unify ~scope env subst x y with
        | None -> None
        | Some (prefix, subst) ->
          match Disequality.recheck env subst ctrs prefix with
          | None      -> None
          | Some ctrs ->
            let next_state = {st with subst; ctrs} in
            if PrunesControl.is_exceeded ()
            then begin
              let () = PrunesControl.reset_cur_counter () in
              match Prunes.recheck (prunes next_state) env subst with
              | Prunes.Violated -> None
              | NonViolated -> Some next_state
            end else begin
(*              print_endline "check skipped";*)
              let () = PrunesControl.incr () in
              Some next_state
            end


    let diseq x y ({env; subst; ctrs; scope} as st) =
      match Disequality.add env subst ctrs x y with
      | None      -> None
      | Some ctrs ->
          match Prunes.recheck (prunes st) env subst with
          | Prunes.Violated -> None
          | NonViolated -> Some {st with ctrs}

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

let only_head g st =
  let stream = g st in
  try Stream.single @@ Stream.hd stream
  with Failure _ -> Stream.nil

let (===) x y st =
  let _t =
    IFDEF STATS THEN
    (let () = unification_incr () in
    Timer.make ())
    ELSE () END
  in

  match State.unify x y st with
  | Some st ->
    let () = IFDEF STATS THEN unification_time_incr _t ELSE () END in
    success st
  | None    ->
    let () = IFDEF STATS THEN unification_time_incr _t ELSE () END in
    failure st

let unify = (===)

let (=/=) x y st =
  match State.diseq x y st with
  | Some st ->
      let () = IFDEF STATS THEN delay_counter_incr () ELSE () END in
      success st
  | None    -> failure st

let diseq = (=/=)

let delay g st = Stream.from_fun (fun () -> g () st)

let conj f g st =
  let () = IFDEF STATS THEN conj_counter_incr () ELSE () END in
  Stream.bind (f st) g

let debug_var v reifier call = fun st ->
  let xs = List.map (fun answ ->
    reifier (Answer.env answ) (Obj.magic @@ Answer.ctr_term answ)
    ) (State.reify v st)
  in
  call xs st

let structural : 'a  ->
  ('a , 'b) Reifier.t ->
  ('b -> bool) ->
  goal = fun term rr k st ->
  let new_constraints = Prunes.extend (State.prunes st) (Obj.magic term) rr k in
  match Prunes.check_last new_constraints (State.env st) (State.subst st) with
  | Prunes.Violated -> failure st
  | NonViolated -> success { st with State.prunes = new_constraints }

(*
include (struct
  @type cost = CFixed of GT.int | CAtLeast of GT.int with show
  let show_cost x = GT.show cost x

  let minimize cost reifier var goalish state =
    let old_cost = ref None in
    goalish var state |> Stream.filter (fun st0 ->
      let reified =
        let env = State.env st0 in
        let s = State.subst st0 in
        reifier env (Obj.magic @@ Subst.apply env s var)
      in
      let c = cost reified in
      match !old_cost with
      | None ->
          Format.printf "setting intial cost %s\n%!" (show_cost c);
          old_cost := Some c;
          true
      | Some old ->
          match (old,c) with
          | CFixed old,CFixed new_ when old < new_ -> false
          | CFixed old, CFixed new_ when old = new_ -> true
          | CFixed old, CFixed new_ ->
              Format.printf "setting cost %s\n%!" (show_cost c);
              old_cost := Some c;
              true

          | CAtLeast old, CFixed new_ when old < new_ -> false
          | CAtLeast old, CFixed new_ ->
              Format.printf "setting cost %s\n%!" (show_cost c);
              old_cost := Some c;
              true

          | CFixed old, CAtLeast new_ when old < new_ -> false
          | CFixed old, CAtLeast new_ -> true

          | CAtLeast old, CAtLeast new_ when old < new_ -> false
          | CAtLeast old, CAtLeast new_ ->
              Format.printf "setting cost %s\n%!" (show_cost c);
              old_cost := Some c;
              true
    )
end : sig
  type cost = CFixed of GT.int | CAtLeast of GT.int

  val minimize : ('b -> cost) ->
    (Env.t -> 'a ilogic -> 'a) ->
    ('a ilogic) ->
    ('logicvar -> goal) -> goal
end)
*)

let (&&&) = conj

(* This is a strightforward implementation
  but in Scheme it is implemented differently
*)
(* let (?&) gs = List.fold_right (&&&) gs success *)

(* This is actual clone of Scheme implementation *)
let (?&) gs st =
  match gs with
  | [h] -> h st
  | h::tl ->
    List.fold_left (fun acc x -> Stream.bind acc x) (h st) tl
  | [] -> assert false
  (* TODO: Maybe introduce a special multiconjunction without corner case?*)

let disj_base f g st = Stream.mplus (f st) (Stream.from_fun (fun () -> g st))

let disj f g st =
  let () = IFDEF STATS THEN disj_counter_incr () ELSE () END in
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

let condo2 a b st =
  let st = State.new_scope st in
  match Stream.msplit @@ a st with
  | Some (a, b) -> Stream.cons a b
  | None -> b st

let call_fresh f st =
  let x = State.fresh st in
  f x st

let wc f st =
  let x = State.wc st in
  f x st
;;


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
    val succ : ('a -> State.t -> 'd) -> ('e ilogic -> 'a) -> State.t -> 'e ilogic * 'd
  end = struct
    let zero f      = f
    let succ prev f = call_fresh (fun logic st -> (logic, prev (f logic) st))
  end

module ReifyTuple = struct
  let one x env = make_rr env x
  let succ prev (x, xs) env = (make_rr env x, prev xs env)
end

module NUMERAL_TYPS = struct
  type ('a, 'c, 'e, 'f, 'g) one = unit ->
           (('a ilogic -> goal) ->
            State.t -> 'a ilogic * State.t Stream.t) *
           ('c ilogic -> Env.t -> 'c reified) * ('e -> 'e) *
           (('f -> 'g) -> 'f -> 'g)
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) two = unit ->
           (('a Logic.ilogic -> 'b Logic.ilogic -> goal) ->
            State.t -> 'a Logic.ilogic * ('b Logic.ilogic * State.t Stream.t)) *
           ('c Logic.ilogic * 'd Logic.ilogic ->
            Env.t -> 'c Logic.reified * 'd Logic.reified) *
           ('e * ('f * 'g) -> ('e * 'f) * 'g) *
           (('h -> 'i -> 'j) -> 'h * 'i -> 'j)
  type ('a,'c,'e,'g,'i,'k,'m,'n,'o,'p,'q,'r,'s,'t) three = unit ->
           (('a ilogic -> 'c ilogic -> 'e ilogic -> goal) ->
            State.t ->
            'a ilogic *
            ('c ilogic *
             ('e ilogic * State.t Stream.t))) *
           ('g ilogic * ('i ilogic * 'k ilogic) ->
            Env.t ->
            'g reified * ('i reified * 'k reified)) *
           ('m * ('n * ('o * 'p)) -> ('m * ('n * 'o)) * 'p) *
           (('q -> 'r -> 's -> 't) -> 'q * ('r * 's) -> 't)

  type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r) four = unit ->
         (('a ilogic -> 'b ilogic -> 'c ilogic -> 'd ilogic -> goal) ->
          State.t ->
          'a ilogic *
          ('b ilogic * ('c ilogic * ('d ilogic * State.t Stream.t)))) *
         ('e ilogic * ('f ilogic * ('g ilogic * 'h ilogic)) ->
          Env.t -> 'e reified * ('f reified * ('g reified * 'h reified))) *
         ('i * ('j * ('k * ('l * 'm))) -> ('i * ('j * ('k * 'l))) * 'm) *
         (('n -> 'o -> 'p -> 'q -> 'r) -> 'n * ('o * ('p * 'q)) -> 'r)


end

let succ n () =
  let adder, app, ext, uncurr = n () in
  (LogicAdder.succ adder, ReifyTuple.succ app, ExtractDeepest.succ ext, Uncurry.succ uncurr)

let one : (_,_,_,_,_) NUMERAL_TYPS.one = fun () ->
   (LogicAdder.(succ zero)), ReifyTuple.one, ExtractDeepest.ext2, Uncurry.one
let two  : (_,_,_,_,_,_,_,_,_,_) NUMERAL_TYPS.two = fun () -> succ one   ()
let three () = succ two   ()
let four () = succ three ()
let five () = succ four  ()

let q     = one
let qr : (_,_,_,_,_,_,_,_,_,_) NUMERAL_TYPS.two = two
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
      let sc = (Curry.succ : (('a -> 'b) -> 'c) -> ((((_) ilogic as 'k) * 'a -> 'b) -> 'k -> 'c)) in
      (sc currier, Uncurry.succ uncurrier)

    let one () = ((Curry.(one) : ((_) ilogic -> _) as 'x -> 'x), Uncurry.one)

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


let reify_in_empty reifier x =
  let st = State.empty () in
  reifier (State.env st) x

let trace_diseq : goal = fun st ->
  Format.printf "%a\n%!" Disequality.pp (State.constraints st);
  success st

