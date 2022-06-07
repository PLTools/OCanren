module L = Stdlib.List

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

type 'history init      = unit -> 'history
type 'history cleaner   = state -> 'history conj -> 'history conj
type 'history updater   = state -> 'history -> 'history call -> 'history
type 'history separator = state -> 'history conj list -> 'history conj list * 'history conj list

type 'history strategy  = {
  init      : 'history init;
  cleaner   : 'history cleaner;
  updater   : 'history updater;
  separator : 'history separator
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

let unfold ((_, f, args) as call, history) { updater } state =
  f args state (updater state history call)

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
  match strategy.separator state conjs with
  | _, []        -> step strategy (And (state, L.map (strategy.cleaner state) conjs))
  | a, (conj::b) ->
    match unfold conj strategy state with
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
  let rec run n stream =
    let stream, answers = step strategy stream in
    let amount = L.length answers in
    if amount > n then take n answers else
      match stream with
      | None        -> answers
      | Some stream -> answers @ run (n - amount) stream in 
  let adder, reifier, ext, uncurrier = m () in
  let args, stream = ext @@ adder f (State.empty ()) (strategy.init ()) in
  let stream, answers = match stream with
                        | None -> None, []
                        | Some stream -> split_answers stream in
  let amount = L.length answers in
  let answers = if amount > n then take n answers else
                  match stream with
                  | None        -> answers
                  | Some stream -> answers @ run (n - amount) stream in
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

let rec separate_by_pred pred list =
  match list with
  | []      -> [], []
  | x :: xs -> 
    if pred x then [], list else
      let l1, l2 = separate_by_pred pred xs in
      x :: l1, l2

(*************************************************************************************************)

let print_stream stream hprinter =
  let print_height s t = sprintf "%d" (term_height s t) in
  let rec print_stream = function
  | Or (a, b)                        -> sprintf "(%s) \\/ (%s)" (print_stream a) (print_stream b)
  | And (s, [])                      -> sprintf "success"
  | And (s, [(name, _, a), h])       -> sprintf "%s(%s) [%s]" name (String.concat ", " (L.map (print_height s) a)) (hprinter h)
  | And (s, ((name, _, a), h) :: xs) -> sprintf "%s(%s) [%s] /\\ %s" name (String.concat ", " (L.map (print_height s) a)) (hprinter h)
                                                                         (print_stream (And (s, xs))) in
  match stream with
  | None        -> "falure"
  | Some stream -> print_stream stream

(*************************************************************************************************)

let lb_strategy =
  let init _        = ()    in
  let cleaner _ c   = c     in
  let updater _ _ _ = ()    in
  let separator _ l = [], l in
  { init; cleaner; updater; separator }

module M = Map.Make(String)

let rec sublist list mask =
  match list, mask with
  | _::xs, false::ys -> sublist xs ys
  | x::xs, true ::ys -> x :: sublist xs ys
  | []   , []        -> []
  | _    , _         -> failwith "sublist: defferent length of list and mask"

(*************************************************************************************************)

let fair_strategy info height =
  
  let is_bound state t =
    let subst = State.subst state in
    let env = State.env state in
    match Term.var t with
    | None   -> true
    | Some v -> Subst.bound_var_check env subst v in

  let init _ = (M.empty, false) in
  
  let cleaner _ (c, _) = c, (M.empty, false) in
  
  let updater s (h, _) (n, _, args) =  
    M.add n (L.map (term_height s) (sublist args (M.find n info))) h, true in
  
  let predicate s ((n, _, args), (h, _)) =
    (* printf "size   : %d\n" (L.length args); *)
    let ess_args = sublist args (M.find n info) in
    (* printf "es size: %d\n" (L.length ess_args); *)
    (* printf "free check: %b\n" (L.exists (is_bound s) ess_args); *)
    L.exists (is_bound s) ess_args && 
    match M.find_opt n h with
    | None -> true
    | Some prev_heights ->
      let act_heights = L.map (term_height s) ess_args in
      L.for_all2 (>=) prev_heights act_heights &&
      L.exists2 (>) prev_heights act_heights in
    
  let separator s l =
    let l1, l2 = separate_by_pred (predicate s) l in
    match l2 with
    | _ :: _ -> l1, l2
    | _ -> separate_by_pred (fun (_, (_, b)) -> not b) l in
   
  { init; cleaner; updater; separator }

(*************************************************************************************************)

open List

let rec appendo_rel x y xy =
((x === nil ()) &&& (y === xy)) |||
call_fresh (fun e -> call_fresh (fun xs -> call_fresh (fun xys -> (?&)
  [(x === e % xs);
  (xy === e % xys);
  (appendo xs y xys)]))) 
and appendo x y xy = call Call.three "appendo" appendo_rel x y xy 


let rec reverso_rel xy yx =
 ((xy === nil ()) &&& (yx === nil ())) |||
 call_fresh (fun e -> call_fresh (fun xys -> call_fresh (fun yxs ->
   (xy === e % xys) &&& 
   (reverso xys yxs) &&&
   (appendo yxs (e % nil ()) yx) 
   
 )))
 and reverso xy yx = call Call.two "reverso" reverso_rel xy yx

let reverso_info = M.of_seq (L.to_seq ["appendo", [true; false; true];
                                       "reverso", [true; false]])

(*************************************************************************************************)

let rec llist = function
| []      -> nil ()
| x :: xs -> !!x % llist xs

(*************************************************************************************************)

(* let _ =
  let x = run 10 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    lb_strategy 
                    (fun a -> reverso (llist [1;2;3]) a) in 
  L.iter (fun a -> printf "lb: %s\n" (show ground (show int) a)) x *)

let _ = 
  let x = run 2 q (fun a -> a#reify (prj_exn Logic.prj_exn)) 
                    (fair_strategy reverso_info term_height) 
                    (fun a -> reverso (llist [1; 2; 3]) a) in 
  L.iter (fun a -> printf "fair: %s\n" (show ground (show int) a)) x

(*************************************************************************************************)

let show_steps goal strategy hprinter n =
  let rec show_step stream m =
    if m > 0 then
      match stream with
      | None -> ()
      | Some stream ->
        let next, ans = step strategy stream in
        printf "step %2d: %s\nanswers: %d\n\n" (n - m + 1) (print_stream next hprinter) (L.length ans);
        show_step next (m - 1) in
  let stream = goal (State.empty ()) (strategy.init ()) in
  printf "step  0: %s\n\n" (print_stream stream hprinter);
  show_step stream n

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

let fair_hprinter (h, b) = 
  let print_args a = String.concat ", " (L.map (sprintf "%d") a) in
  let th = String.concat "; " (L.map (fun (n, a) -> sprintf "%s(%s)" n (print_args a)) (L.of_seq (M.to_seq h))) in
  sprintf "%s | %b" th b

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