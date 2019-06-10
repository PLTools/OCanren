(*
 * MiniKanrenCode: miniKanren implementation.
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

open Printf

@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

let logic = {logic with
  gcata = ();
  plugins =
    object
      method gmap      = logic.plugins#gmap
      method html      = logic.plugins#html
      method eq        = logic.plugins#eq
      method compare   = logic.plugins#compare
      method foldl     = logic.plugins#foldl
      method foldr     = logic.plugins#foldr
      method show fa x =
        GT.transform(logic)
          (GT.lift fa)
          (object inherit ['a] @logic[show]
             method c_Var _ s i cs =
               let c = match cs with
               | [] -> ""
               | _  -> sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
               in
               sprintf "_.%d%s" i c
             method c_Value _ _ x = x.GT.fx ()
           end)
          ()
          x
    end
}

let (!!!) = Obj.magic

module Binding :
  sig
    type t =
      { var   : Var.t
      ; term  : Term.t
      }

    val is_relevant : Env.t -> VarSet.t -> t -> bool

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end =
  struct
    type t =
      { var   : Var.t
      ; term  : Term.t
      }

    let is_relevant env vs {var; term} =
      (VarSet.mem var vs) ||
      (match Env.var env term with Some v -> VarSet.mem v vs | None -> false)

    let equal {var=v; term=t} {var=u; term=p} =
      (Var.equal v u) || (Term.equal t p)

    let compare {var=v; term=t} {var=u; term=p} =
      let res = Var.compare v u in
      if res <> 0 then res else Term.compare t p

    let hash {var; term} = Hashtbl.hash (Var.hash var, Term.hash term)
  end

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
    val disequality : t -> Binding.t list

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
        ~fvar:(fun v -> Term.repr {v with Var.constraints = []})

    let ctr_term (_, t) = t

    let disequality (env, t) =
      let rec helper acc x =
        Term.fold x ~init:acc
          ~fval:(fun acc _ -> acc)
          ~fvar:(fun acc var ->
            ListLabels.fold_left var.Var.constraints ~init:acc
              ~f:(fun acc ctr_term ->
                let ctr_term = Term.repr ctr_term in
                let var = {var with Var.constraints = []} in
                let term = unctr_term @@ (env, ctr_term) in
                let acc = Binding.({var; term})::acc in
                helper acc ctr_term
              )
          )
      in
      helper [] t

    let lift env' (env, t) =
      let vartbl = VarTbl.create 31 in
      let rec helper x =
        Term.map x
          ~fval:(fun x -> Term.repr x)
          ~fvar:(fun v -> Term.repr @@
            try
              VarTbl.find vartbl v
            with Not_found ->
              let new_var = Env.fresh ~scope:Var.non_local_scope env' in
              VarTbl.add vartbl v new_var;
              {new_var with Var.constraints =
                List.map (fun x -> helper x) v.Var.constraints
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

module State :
  sig

  end = struct

  end

type 'a goal' = State.t -> 'a

type goal = State.t Stream.t goal'

let success st = Stream.single st
let failure _  = Stream.nil

let (===) x y st =
  match State.unify x y st with
  | Some st -> success st
  | None    -> failure st

let (=/=) x y st =
  match State.diseq x y st with
  | Some st -> success st
  | None    -> failure st

let delay g st = Stream.from_fun (fun () -> g () st)

let conj f g st = Stream.bind (f st) g
let (&&&) = conj
let (?&) gs = List.fold_right (&&&) gs success

let disj_base f g st = Stream.mplus (f st) (Stream.from_fun (fun () -> g st))

let disj f g st = let st = State.new_scope st in disj_base f g |> (fun g -> Stream.from_fun (fun () -> g st))

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

include (struct

type ('a, 'b) injected = 'a
type ('a, 'b) inj = ('a,'b) injected
type helper = Env.t
(* camlp5  doesn't support nonrec, so dirty hack *)
type 'a logic_ = 'a logic
type 'a logic = 'a logic_

include MiniKanrenTypes.Ts

let rec reify env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

let rec prjc of_int env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (prjc of_int env) v in of_int i cs
  | None   -> Obj.magic x

module Fmap (T : T1) =
  struct
    external distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected = "%identity"

    let rec reify r env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r env) v in Var (i, cs)
      | None   -> Value (T.fmap (r env) x)

    let rec prjc r of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r of_int env) v in of_int i cs
      | None   -> T.fmap (r env) x
  end

module Fmap2 (T : T2) =
  struct
    external distrib : (('a,'b) injected, ('c, 'd) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected = "%identity"

    let rec reify r1 r2 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) x)

    let rec prjc r1 r2 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) x
  end

module Fmap3 (T : T3) =
  struct
    external distrib : (('a, 'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected = "%identity"

    let rec reify r1 r2 r3 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 r3 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) x)

    let rec prjc r1 r2 r3 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) (r3 env) x
end

module Fmap4 (T : T4) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                     (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x)

  let rec prjc r1 r2 r3 r4 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x
end

module Fmap5 (T : T5) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x)

  let rec prjc r1 r2 r3 r4 r5 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x
end

module Fmap6 (T : T6) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 r6 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 r6 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x)

  let rec prjc r1 r2 r3 r4 r5 r6 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 r6 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x
end

end : sig
  type ('a, 'b) injected

  val reify : Env.t -> ('a, 'a logic) injected -> 'a logic

  val prjc : (int -> 'a list -> 'a) -> Env.t -> ('a, 'a logic) injected -> 'a

  include module type of struct include MiniKanrenTypes.Ts end

  include MiniKanrenTypes.Fmaps with type ('a,'b) inj = ('a,'b) injected
                                 and type helper = Env.t
                                 and type 'a logic_ = 'a logic

end)

external lift : 'a -> ('a, 'a) injected                      = "%identity"
external inj  : ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"

exception Not_a_value

let to_logic x = Value x

let from_logic = function
| Value x    -> x
| Var (_, _) -> raise Not_a_value

let (!!) x = inj (lift x)

class type ['a,'b] reified = object
  method is_open : bool
  method prj     : 'a
  method reify   : (Env.t -> ('a, 'b) injected -> 'b) -> 'b
  method prjc    : (Env.t -> ('a, 'b) injected -> 'a) -> 'a
end

let make_rr : Env.t -> ('a, 'b) injected -> ('a, 'b) reified  = fun env x ->
  object (self)
    method is_open            = Env.is_open env x
    method prj                = if self#is_open then raise Not_a_value else !!!x
    method reify reifier      = reifier env x
    method prjc  onvar        = onvar   env x
  end

let prj x = let rr = make_rr (Env.empty ()) x in rr#prj

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
                    ~f:(let open Binding in fun acc {var; term} ->
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
      let env = Env.create ~anchor:Var.tabling_env in
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
