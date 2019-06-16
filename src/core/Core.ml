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