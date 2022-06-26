module L = Stdlib.List
module M = Map.Make(String)

open Format

open Term
open Core
open Logic

open GT

(*************************************************************************************************)

let rec take n list =
  if n <= 0 then [] else
  match list with
  | []      -> failwith "take"
  | x :: xs -> x :: take (n-1) xs

(*************************************************************************************************)

type state              = State.t
type term               = Obj.t

type 'history call      = string * (term list -> 'history goal) * term list

and  'history conj      = 'history call * 'history

and  'history stream'   = Or  of 'history stream' * 'history stream'
                        | And of state * 'history conj list
and  'history stream    = 'history stream' option

and  'history goal      = state -> 'history -> 'history stream

type 'history init                    = unit -> 'history
type 'history cleaner                 = state -> 'history conj -> 'history conj
type ('step_info, 'history) updater   = state -> 'history -> 'history call -> 'step_info -> 'history
type ('step_info, 'history) separator = state -> 'history conj list -> 'history conj list * 'history conj list * 'step_info

type ('step_info, 'history) strategy  = {
  blank_info : unit -> 'step_info;
  init       : 'history init;
  cleaner    : 'history cleaner;
  updater    : ('step_info, 'history) updater;
  separator  : ('step_info, 'history) separator
}

type fair_history = { 
  hist       : int list M.t;
  unfolded   : bool;
  need_clean : bool
}

(*************************************************************************************************)

module ToList = struct
  let one f x    = f [Obj.repr x]
  let succ n f x = n (fun xs -> f (Obj.repr x :: xs))

  let two   = succ one
  let three = succ two
  let four  = succ three
  let five  = succ four
  let six   = succ five
  let seven = succ six
end

module FromList = struct
  let one    f = function
    | [x]   -> f (Obj.obj x)
    | _     -> failwith "Uncurry.one: should not happen"

  let succ n f = function
    | x::xs -> n (f (Obj.obj x)) xs
    | _     -> failwith "Uncurry.succ: should not happen"

  let two   = succ one
  let three = succ two
  let four  = succ three
  let five  = succ four
  let six   = succ five
  let seven = succ six
end

module Call = struct
  let one () : ((Obj.t list -> 'a) -> 'b -> 'a) * (('b -> 'a) -> Obj.t list -> 'a) =
    (ToList.one, FromList.one)
  let succ n () =
    let curry, uncurry = n () in
    (ToList.succ curry, FromList.succ uncurry)

  let two   () = succ one   ()
  let three () = succ two   ()
  let four  () = succ three ()
  let five  () = succ four  ()
  let six   () = succ five  ()
  let seven () = succ six   ()

  let create n name (f : 'a) : 'a =
    let (curry, uncurry) = n () in
    curry (fun l state history -> Some (And (state, [(name, uncurry f, l), history])))
end

(*************************************************************************************************)

let merge a b =
  match a, b with
  | None,   None   -> None
  | Some _, None   -> a
  | None,   Some _ -> b
  | Some a, Some b -> Some (Or (a, b))

let rec push stream cs1 cs2 =
  match stream with
  | Or (a, b)    -> Or (push a cs1 cs2, push b cs1 cs2)
  | And (st, cs) -> And (st, cs1 @ cs @ cs2)

let app_to_stream (f : 'history goal) stream history =
  let rec app stream =
    match stream with
    | Or  (a, b)         -> merge (app a) (app b)
    | And (state, calls) ->
      match f state history with
      | None        -> None
      | Some stream -> Some (push stream calls [])
  in match stream with
  | None        -> None
  | Some stream -> app stream

(*************************************************************************************************)

let (===) a b state _ =
  match State.unify a b state with
  | None    -> None
  | Some st -> Some (And (st, []))

let (=/=) a b state _ =
  match State.diseq a b state with
  | None    -> None
  | Some st -> Some (And (st, []))

let call_fresh f state history = f (State.fresh state) state history

let disj_base a b state history = merge (a state history) (b state history)

let (|||) a b state history = 
  let state = State.new_scope state in
  disj_base a b state history

let conde gs st history =
  let st = State.new_scope st in
  let rec inner = function
    | [g]   -> g
    | g::gs -> disj_base g (inner gs)
    | []    -> failwith "Wrong argument of (?!)"
  in  inner gs st history

let (&&&) a b state history = app_to_stream b (a state history) history

let (?&) gs =
  match gs with
  | [h]   -> h
  | h::tl -> L.fold_left (&&&) h tl
  | []    -> failwith "Wrong argument of (?&)"

let call n name f = Call.create n name f

(*************************************************************************************************)

let unfold ((name, f, args) as call, history) { updater } info state =
  f args state (updater state history call info)

let rec split_answers = function
| Or (a, b) ->
  let a, ans1 = split_answers a in
  let b, ans2 = split_answers b in
  merge a b, ans1 @ ans2
| And (state, []) -> None, [state]
| stream          -> Some stream, []

let rec step strategy = function
| Or (a, b) -> 
  let stream, answers = step strategy a in
  merge (Some b) stream, answers
| And (state, conjs) ->
  let is_empty = function
    | [] -> true
    | _  -> false in
  match conjs with
  | [conj] -> 
    begin match unfold conj strategy (strategy.blank_info ()) state with
    | None        -> None, []
    | Some stream -> split_answers stream
  end
  | _ ->
    match strategy.separator state conjs with
    | _, [], _           -> step strategy (And (state, L.map (strategy.cleaner state) conjs))
    | a, (conj::b), info ->
      match unfold conj strategy info state with
      | None                                      -> None, []
      | Some stream when is_empty a && is_empty b -> split_answers stream
      | Some stream                               -> Some (push stream a b), []

(*************************************************************************************************)

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module LogicAdder :
  sig
    val zero : 'a goal -> 'a goal
    val succ : ('a -> state -> 'h -> 'd) -> ('e ilogic -> 'a) -> state -> 'h ->'e ilogic * 'd
  end = struct
    let zero f      = f
    let succ prev f = call_fresh (fun logic st h -> (logic, prev (f logic) st h))

  end

module ReifyTuple = struct
  let one x env = make_rr env x
  let succ prev (x, xs) env = (make_rr env x, prev xs env)
end

module Uncurry =
  struct
    let one = (@@)
    let succ k f (x,y) = k (f x) y
  end

let succ n () =
  let adder, app, ext, uncurrier = n () in
  LogicAdder.succ adder, ReifyTuple.succ app, ExtractDeepest.succ ext, Uncurry.succ uncurrier

let one = fun () -> (LogicAdder.(succ zero)), ReifyTuple.one, ExtractDeepest.ext2, Uncurry.one
let two = fun () -> succ one ()
let three () = succ two ()
let four () = succ three ()
let five () = succ four ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let qrstu = five

(*************************************************************************************************)

let run n m r strategy f =
  let rec run n stream acc =
    let stream, answers = step strategy stream in
    let amount = L.length answers in
    if amount >= n then take n answers @ acc else
      match stream with
      | None        -> answers @ acc
      | Some stream -> run (n - amount) stream (answers @ acc) in 
  let adder, reifier, ext, uncurrier = m () in
  let args, stream = ext @@ adder f (State.empty ()) (strategy.init ()) in
  let stream, answers = match stream with
                        | None -> None, []
                        | Some stream -> split_answers stream in
  let amount = L.length answers in
  let answers = if amount >= n then take n answers else
                  match stream with
                  | None        -> answers
                  | Some stream -> run (n - amount) stream answers in
  let answers = L.concat_map (fun answ -> State.reify args answ) answers in
  L.map (fun a -> uncurrier r (reifier (Obj.magic @@ Answer.ctr_term a) (Answer.env a))) answers

(*************************************************************************************************)

let term_size state t =
  Subst.size (State.env state) (State.subst state) (repr t)
    ~fvar:(fun v -> 1)
    ~fval:(fun v -> 1)
    ~fbox:(fun v -> 1)
    ~aggr:(+)
    ~zero:0

let term_height state t =
  Subst.size (State.env state) (State.subst state) (repr t)
    ~fvar:(fun v -> 1)
    ~fval:(fun v -> 1)
    ~fbox:(fun v -> 1)
    ~aggr:max
    ~zero:0

(*************************************************************************************************)

let print_stream stream term_height hprinter =
  let print_height s t = sprintf "%d" (term_height s t) in
  let rec print_stream = function
  | Or (a, b)                        -> sprintf "%s\n\\/\n     %s" (print_stream a) (print_stream b)
  | And (s, [])                      -> sprintf "success"
  | And (s, [(name, _, a), h])       -> sprintf "%s(%s) [%s]" name (String.concat ", " (L.map (print_height s) a)) (hprinter h)
  | And (s, ((name, _, a), h) :: xs) -> sprintf "%s(%s) [%s] \n  /\\ %s" name (String.concat ", " (L.map (print_height s) a)) (hprinter h)
                                                                         (print_stream (And (s, xs))) in
  match stream with
  | None        -> "falure"
  | Some stream -> print_stream stream

let fair_hprinter h = 
  let print_args a = String.concat ", " (L.map (sprintf "%d") a) in
  let hist_list = L.of_seq (M.to_seq h.hist) in
  sprintf "hist (%d) = %s |  unfolded = %b | need_clean = %b"
   (M.cardinal h.hist) 
   (String.concat "; " (L.map (fun (n, a) -> sprintf "%s(%s)" n (print_args a)) hist_list))
   h.unfolded
   h.need_clean

let show_steps goal strategy term_height hprinter n =
  let rec show_step stream m =
    if m > 0 then
      match stream with
      | None -> ()
      | Some stream ->
        let next, ans = step strategy stream in
        printf "step %2d:\n     %s\nanswers: %d\n\n" (n - m + 1) (print_stream next term_height hprinter) (L.length ans);
        show_step next (m - 1) in
  let stream = goal (State.empty ()) (strategy.init ()) in
  printf "step  0:\n     %s\n\n" (print_stream stream term_height hprinter);
  show_step stream n

(*************************************************************************************************)

let lb_strategy =
  let blank_info _    = ()    in
  let init _          = ()    in
  let cleaner _ c     = c     in
  let updater _ _ _ _ = ()    in
  let separator _ l   = [], l, () in
  { blank_info; init; cleaner; updater; separator }

(*************************************************************************************************)

let rec sublist list mask =
  match list, mask with
  | _::xs, false::ys -> sublist xs ys
  | x::xs, true ::ys -> x :: sublist xs ys
  | []   , []        -> []
  | _    , _         -> failwith "sublist: defferent length of list and mask"

let fair_strategy info height =
  
  let is_bound state t =
    let subst = State.subst state in
    let env = State.env state in
    match Term.var t with
    | None   -> true
    | Some v -> Subst.bound_var_check env subst v in

  let blank_info _ = None in

  let init _ = { hist       = M.empty;
                 unfolded   = false;
                 need_clean = false 
               } in
  
  let cleaner h (c, _) = c, init h in
  
  let updater s h (n, _, args) last =
    if h.need_clean then h else
    let args =
      match last with
      | None   -> L.map (height s) (sublist args (M.find n info))
      | Some i -> i in
    { h with hist = M.add n args h.hist; unfolded = true } in
  
  let check_one s ((n, _, args), h) =
    if h.need_clean then h, None, true else
    let ess_args = 
      try sublist args (M.find n info)
      with Failure s -> failwith (sprintf "check_one (fun: %s), %s" n s) in
    match ess_args with
    | [] -> h, None, true
    | _ ->
    let has_bounded_arg = L.exists (is_bound s) ess_args in
    if has_bounded_arg then
      match M.find_opt n h.hist with
      | None -> h, None, has_bounded_arg
      | Some prev_heights ->
        let act_heights = L.map (height s) ess_args in
        let need_clean =
          L.exists2 (>) act_heights prev_heights ||
          L.for_all2 (>=) act_heights prev_heights in
          { h with need_clean }, Some act_heights, has_bounded_arg
      else h, None, has_bounded_arg in
    
  let separate_by_pred pred list =
    let rec separate_by_pred pred list acc =
      match list with
      | []      -> acc, [], None
      | ((c, _) as x) :: xs ->
        let h, last, has_bounded_arg = pred x in
        let x = (c, h) in
        if has_bounded_arg && not h.need_clean then acc, x :: xs, last else
          separate_by_pred pred xs (x :: acc) in
    separate_by_pred pred list [] in

  let rec first_not_unfolded l = 
    match l with
    | []      -> [], []
    | x :: xs -> 
      if (snd x).unfolded then
        let l1, l2 = first_not_unfolded xs in x :: l1, l2
      else [], l in


  let separator s l =
    let l1, l2, last = separate_by_pred (check_one s) l in
    match l2 with
    | _ :: _ -> l1, l2, last
    | _ -> let l1, l2 = first_not_unfolded l in
           l1, l2, None in
   
  { blank_info; init; cleaner; updater; separator }


(*************************************************************************************************)

open Benchmark
(* let _ =
  let n : int64 = 100L in

  (* let strategy = fair_strategy reverso_info term_height in *)
  let strategy = lb_strategy in

  let f = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) strategy in
  let _ = latency1 n f (fun a -> reverso (llist (L.init 50 (fun i -> i))) a) in

  let strategy = fair_strategy reverso_info term_height in

  let f = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) strategy in
  latency1 n f (fun a -> reverso (llist (L.init 50 (fun i -> i))) a) *)

(* let _ = 
  let x = run 1 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    lb_strategy 
                    (fun a -> reverso a (llist (L.init 0 (fun i -> i)))) in 
  L.iter (fun a -> printf "fair: %s\n" (show ground (show int) a)) x *)

(*************************************************************************************************)
(*************************************************************************************************)
(*************************************************************************************************)

(* lb test: appendo l [] l *)
(* let _ = 
  let goal = call_fresh (fun l -> appendo l (nil ()) l) in
  show_steps goal lb_strategy (fun _ -> "") 10 *)

(*************************************************************************************************)

(* lb test: reverso [1;2;3] l *)
(* let _ = 
  let goal = call_fresh (fun l -> reverso (llist [1;2;3]) l) in
  show_steps goal lb_strategy (fun _ -> "") 100 *)

(*************************************************************************************************)

(* lb test: reverso l [1;2;3] *)
(* let _ = 
  let goal = call_fresh (fun l -> reverso l (llist [1;2;3;4;5;6;7])) in
  show_steps goal lb_strategy (fun _ -> "") 1000 *)

(*************************************************************************************************)

(* f test: reverso l [1;2;3] *)
(* let _ =
  let strategy = fair_strategy reverso_info term_height in
  let goal = call_fresh (fun l -> reverso (llist [1;2;3]) l) in
  show_steps goal strategy fair_hprinter 100 *)

(* f test: reverso [1;2;3] l *)

(* let _ =
  let strategy = fair_strategy reverso_info term_height in
  let goal = call_fresh (fun l -> reverso l (llist [1;2;3;4;5;6;7])) in
  show_steps goal strategy fair_hprinter 100 *)


(*************************************************************************************************)

  (* let _ =
  let strategy = fair_strategy sorto_info term_height in
  (* let strategy = lb_strategy in *)
  let goal = call_fresh (fun a -> sort a (int_list2lnat_llist [0; 1; 2])) in
  show_steps goal strategy fair_hprinter 1000 *)
  (* show_steps goal strategy (fun _ -> "") 1000 *)
