module Stream =
  struct

    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.lazy_from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec retrieve ?(n=(-1)) s =
      if n = 0
      then [], s
      else match s with
           | Nil          -> [], s
           | Cons (x, xs) -> let xs', s' = retrieve ~n:(n-1) xs in x::xs', s'
	   | Lazy  z      -> retrieve ~n:n (Lazy.force z)            

    let take ?(n=(-1)) s = retrieve ~n:n s

    let rec mplus fs gs =
      from_fun (fun () ->
         match fs with
         | Nil           -> gs
         | Cons (hd, tl) -> cons hd (mplus gs tl) 
	 | Lazy z        -> mplus gs (Lazy.force z)
      )

    let rec bind xs f =
      from_fun (fun () ->
        match xs with
        | Cons (x, xs) -> mplus (f x) (bind xs f)
	| Nil          -> nil
        | Lazy z       -> bind (Lazy.force z) f
     )

    let rec map f = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
    | Lazy s -> Lazy (Lazy.lazy_from_fun (fun () -> map f @@ Lazy.force s))

  end

let (!!) = Obj.magic;;

type var = GT.int

@type 'a logic = Var of GT.int * 'a logic GT.list | Value of 'a with show, html, eq, compare, foldl, foldr, gmap

let logic = {
  logic with plugins = 
    object 
      method html    = logic.plugins#html
      method eq      = logic.plugins#eq
      method compare = logic.plugins#compare
      method foldr   = logic.plugins#foldr
      method foldl   = logic.plugins#foldl
      method gmap    = logic.plugins#gmap    
      method show fa x = 
        GT.transform(logic) 
           (GT.lift fa) 
           (object inherit ['a] @logic[show]              
              method c_Var   _ _ i _ = Printf.sprintf "_.%d" i
              method c_Value _ _ x = x.GT.fx ()
            end) 
           () 
           x
    end
};;

@type 'a llist = Nil | Cons of 'a logic * 'a llist logic with show, html, eq, compare, foldl, foldr, gmap

let (!) x = Value x

let (%)  x y = !(Cons (x, y))
let (%<) x y = !(Cons (x, !(Cons (y, !Nil))))
let (!<) x   = !(Cons (x, !Nil))

let rec of_list = function
| [] -> !Nil
| x::xs -> !x % (of_list xs)

exception Not_a_value 
exception Occurs_check

let rec to_listk k = function
| Value Nil -> []
| Value (Cons (Value x, xs)) -> x :: to_listk k xs
| z -> k z

let to_value = function Var _ -> raise Not_a_value | Value x -> x

let to_list l = to_listk (fun _ -> raise Not_a_value) l

let llist = {
  llist with plugins = 
    object 
      method html    = llist.plugins#html
      method eq      = llist.plugins#eq
      method compare = llist.plugins#compare
      method foldr   = llist.plugins#foldr
      method foldl   = llist.plugins#foldl
      method gmap    = llist.plugins#gmap    
      method show fa x = "[" ^
        (GT.transform(llist) 
           (GT.lift fa) 
           (object inherit ['a] @llist[show]              
              method c_Nil   _ _      = ""
              method c_Cons  i s x xs = GT.show(logic) fa x ^ (match xs with Value Nil -> "" | _ -> "; " ^ GT.show(logic) (s.GT.f i) xs)
            end) 
           () 
           x
        ) ^ "]"
    end
}

type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let rec wrap (x : Obj.t) =
  Obj.(
    let is_valid_tag =
      List.fold_left
      (fun f t tag -> tag <> t && f tag)
      (fun _ -> true)
      [lazy_tag   ; closure_tag  ; object_tag  ; infix_tag ;
       forward_tag; no_scan_tag  ; abstract_tag; custom_tag;
       final_tag  ; unaligned_tag; out_of_heap_tag
      ]
    in
    let is_unboxed obj =
      is_int obj ||
      (fun t -> t = string_tag || t = double_tag) (tag obj)
    in
    if is_unboxed x
    then Unboxed x
    else
      let t = tag x in
      if is_valid_tag t
      then
	let f = if t = double_array_tag then !! double_field else field in
	Boxed (t, size x, f x)
      else Invalid t
    )

let generic_show x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner o =
    match wrap o with
    | Invalid n             -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed n when !!n=0  -> Buffer.add_string b "[]"
    | Unboxed n             -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!n))
    | Boxed (t,l,f) when t=0 && l=1 && (match wrap (f 0) with Unboxed i when !!i >=10 -> true | _ -> false) ->
       Printf.bprintf b "var%d" (match wrap (f 0) with Unboxed i -> !!i | _ -> failwith "shit")

    | Boxed   (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : t -> 'a logic * t
    val var    : t -> 'a logic -> int option
    val vars   : t -> unit logic list 
    val show   : t -> string
  end = 
  struct
    module H = Hashtbl.Make (
      struct
        type t = unit logic
        let hash = Hashtbl.hash
        let equal = (==)
      end)

    type t = unit H.t * int

    let counter_start = 10 (* 1 to be able to detect empty list *)
    let empty () = (H.create 1024, counter_start)

    let fresh (h, current) =
      let v = Var (current, []) in
      H.add h v ();
      (!!v, (h, current+1))

    let var (h, _) x =
      if H.mem h (!! x)
      then let Var (i, _) = !! x in Some i
      else None

    let vars (h, _) = H.fold (fun v _ acc -> v :: acc) h []

    let show env = (List.fold_left (fun acc (Var (i, _)) -> acc ^ (Printf.sprintf "$%d; " i)) "env {" (vars env)) ^ "}"

  end

module Subst :
  sig
    type t

    val empty   : t

    val of_list : (int * Obj.t * Obj.t) list -> t 
    val split   : t -> Obj.t list * Obj.t list 
    val walk    : Env.t -> 'a logic -> t -> 'a logic
    val walk'   : Env.t -> 'a logic -> t -> 'a logic
    val unify   : Env.t -> 'a logic -> 'a logic -> t option -> (int * Obj.t * Obj.t) list * t option
    val show    : t -> string
  end =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    type t = (Obj.t * Obj.t) M.t

    let show m = (M.fold (fun i (_, x) s -> s ^ Printf.sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}"

    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, v, t) -> M.add i (v, t) s) empty l

    let split s = M.fold (fun _ (x, t) (xs, ts) -> x::xs, t::ts) s ([], []) 

    let rec walk env var subst =
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (snd (M.find i (!! subst))) subst with Not_found -> var

    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None -> 
         let wy = wrap (Obj.repr y) in
	 match wy with
	 | Unboxed _ -> false
	 | Invalid n -> invalid_arg (Printf.sprintf "Invalid value in occurs check (%d)" n)
	 | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
	      else occurs env xi (!!(f i)) subst || inner (i+1)
	    in
	    inner 0

    let rec walk' env var subst =
      match Env.var env var with
      | None ->
	  (match wrap (Obj.repr var) with
	   | Unboxed _ -> !!var
	   | Boxed (t, s, f) ->
               let var = Obj.dup (Obj.repr var) in
               let sf =
		 if t = Obj.double_array_tag
		 then !! Obj.set_double_field
		 else Obj.set_field
	       in
	       for i = 0 to s - 1 do
                 sf var i (!!(walk' env (!!(f i)) subst))
               done;
	       !!var
	   | Invalid n -> invalid_arg (Printf.sprintf "Invalid value for reconstruction (%d)" n)
          )

      | Some i ->
	  (try walk' env (snd (M.find i (!! subst))) subst
	   with Not_found -> !!var
	  )

    let unify env x y subst =
      let rec unify x y (delta, subst) = 
        let extend xi x term delta subst =
          if occurs env xi term subst then raise Occurs_check
          else (xi, !!x, !!term)::delta, Some (!! (M.add xi (!!x, term) (!! subst)))
        in
        match subst with
        | None -> delta, None
        | (Some subst) as s ->
            let x, y = walk env x subst, walk env y subst in
            match Env.var env x, Env.var env y with
            | Some xi, Some yi -> if xi = yi then delta, s else extend xi x y delta subst 
            | Some xi, _       -> extend xi x y delta subst 
	    | _      , Some yi -> extend yi y x delta subst 
	    | _ ->
	        let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
                (match wx, wy with
                 | Unboxed vx, Unboxed vy -> if vx = vy then delta, s else delta, None
                 | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy
	  	    then
		      let rec inner i (delta, subst) = 
			match subst with
                        | None -> delta, None
                        | Some _ ->
  	                   if i < sx
		           then inner (i+1) (unify (!!(fx i)) (!!(fy i)) (delta, subst))
		           else delta, subst
                      in
		      inner 0 (delta, s)
                    else delta, None
	         | Invalid n, _
                 | _, Invalid n -> invalid_arg (Printf.sprintf "Invalid values for unification (%d)" n)
	         | _ -> delta, None
	        )
      in
      unify x y ([], subst)

  end

module State =
  struct  
    type t = Env.t * Subst.t * Subst.t list
    let empty () = (Env.empty (), Subst.empty, [])
    let env   (env, _, _) = env
    let show  (env, subst, constr) = Printf.sprintf "st {%s, %s, %s}" (Env.show env) (Subst.show subst) (GT.show(GT.list) Subst.show constr)
  end

type goal = State.t -> State.t Stream.t

let call_fresh f (env, subst, constr) =
  let x, env' = Env.fresh env in
  f x (env', subst, constr)

exception Disequality_violated

let (===) x y (env, subst, constr) =
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    begin match subst' with
    | None -> Stream.nil
    | Some s -> 
        try
          (* TODO: only apply constraints with the relevant vars *)
          let constr' =
            List.fold_left (fun css' cs -> 
              let x, t  = Subst.split cs in
	      try
                let p, s' = Subst.unify env (!!x) (!!t) subst' in
                match s' with
	        | None -> css'
	        | Some _ ->
                    match p with
	            | [] -> raise Disequality_violated
	            | _  -> (Subst.of_list p)::css'
	      with Occurs_check -> css'
            ) 
            []
            constr
	  in
          Stream.cons (env, s, constr') Stream.nil
        with Disequality_violated -> Stream.nil
    end
  with Occurs_check -> Stream.nil

let (=/=) x y ((env, subst, constr) as st) =
  let normalize_store prefix constr =
    let subst  = Subst.of_list prefix in
    let prefix = List.split (List.map (fun (_, x, t) -> (x, t)) prefix) in
    let subsumes subst (vs, ts) = 
      try 
        match Subst.unify env !!vs !!ts (Some subst) with
	| [], Some _ -> true
        | _ -> false
      with Occurs_check -> false
    in
    let rec traverse = function
    | [] -> [subst]
    | (c::cs) as ccs -> 
	if subsumes subst (Subst.split c)
	then ccs
        else if subsumes c prefix
             then traverse cs
             else c :: traverse cs
    in
    traverse constr
  in
  try 
    let prefix, subst' = Subst.unify env x y (Some subst) in
    match subst' with
    | None -> Stream.cons st Stream.nil
    | Some s -> 
        (match prefix with
        | [] -> Stream.nil
        | _  -> Stream.cons (env, subst, normalize_store prefix constr) Stream.nil
        )
  with Occurs_check -> Stream.cons st Stream.nil

let conj f g st = Stream.bind (f st) g

let (&&&) = conj

let disj f g st = Stream.mplus (f st) (g st)

let (|||) = disj 

let rec (?|) = function
| [h]  -> h
| h::t -> h ||| ?| t

let rec (?&) = function
| [h]  -> h
| h::t -> h &&& ?& t

let conde = (?|)

let rec refine : 'a . State.t -> 'a logic -> 'a logic = fun ((e, s, c) as st) x ->
  let rec walk' env var subst =
    match Env.var env (Subst.walk env var subst) with
    | None ->
        (match wrap (Obj.repr var) with
         | Unboxed _ -> !!var
         | Boxed (t, s, f) ->
            let var = Obj.dup (Obj.repr var) in
            let sf =
              if t = Obj.double_array_tag
              then !! Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to s - 1 do
              sf var i (!!(walk' env (!!(f i)) subst))
           done;
           !!var
         | Invalid n -> invalid_arg (Printf.sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i -> 
        (match var with
         | Var (i, _) -> 
            let cs = 
	      List.fold_left 
		(fun acc s -> 
		   match Subst.walk' env (!!var) s with
		   | Var (j, _) when i = j -> acc
		   | t -> (refine st t) :: acc
		)	
		[]
		c
	    in
	    Var (i, cs)
        )
  in
  walk' e (!!x) s

module ExtractDeepest = 
  struct
    let ext2 x = x 

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module ApplyTuple = 
  struct
    let one arg x = x arg

    let succ prev = fun arg (x, y) -> (x arg, prev arg y)
  end

module ApplyLatest = 
  struct
    let two = (ApplyTuple.one, ExtractDeepest.ext2)

    let apply (appf, extf) tup =
      let x, base = extf tup in
      appf base x

    let succ (appf, extf) = (ApplyTuple.succ appf, ExtractDeepest.succ extf) 
  end

module Uncurry = 
  struct
    let succ k f (x,y) = k (f x) y
  end

type 'a reifier = State.t Stream.t -> 'a logic Stream.t

let reifier : 'a logic -> 'a reifier = fun x ans ->
  Stream.map (fun st -> refine st x) ans

module LogicAdder = 
  struct
    let zero f = f
 
    let succ (prev: 'a -> State.t -> 'b) (f: 'c logic -> 'a) : State.t -> 'c reifier * 'b =
      call_fresh (fun logic st -> (reifier logic, prev (f logic) st))
  end

let one () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyLatest.two

let succ n () = 
  let adder, currier, app = n () in
  (LogicAdder.succ adder, Uncurry.succ currier, ApplyLatest.succ app)

let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let pqrst = five

let run n goalish f =
  let adder, currier, app_num = n () in
  let run f = f (State.empty ()) in
  run (adder goalish) |> ApplyLatest.apply app_num |> (currier f)

