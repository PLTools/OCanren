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

    let take ?(n=(-1)) s = fst @@ retrieve ~n:n s

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

let (!!!) = Obj.magic;;

@type 'a logic = Var of GT.int GT.list * GT.int * 'a logic GT.list | Value of 'a with show, html, eq, compare, foldl, foldr, gmap

let logic = {logic with 
  gcata = (); 
  plugins = 
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
              method c_Var _ s _ i cs = 
                let c =
		  match cs with 
		  | [] -> ""
                  | _  -> Printf.sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
		in
                Printf.sprintf "_.%d%s" i c
                
              method c_Value _ _ x = x.GT.fx ()
            end) 
           () 
           x
    end
};;

@type 'a unlogic = [`Var of GT.int * 'a logic GT.list | `Value of 'a] with show, html, eq, compare, foldl, foldr, gmap

let destruct = function
| Var (_, i, c) -> `Var (i, c)
| Value x       -> `Value x

exception Not_a_value 

let (!!) x = Value x
let inj = (!!)

let prj_k k = function Value x -> x | v -> k v
let prj x = prj_k (fun _ -> raise Not_a_value) x

let (!?) = prj

module TopList = List

module List =
  struct
 
    @type ('a, 'l) t = Nil | Cons of 'a * 'l with show, html, eq, compare, foldl, foldr, gmap

    type 'a logic' = 'a logic

    let logic' = logic

    type 'a ground = ('a, 'a ground) t
    type 'a logic  = ('a, 'a logic)  t logic'

    let (%)  x y = !!(Cons (x, y))
    let (%<) x y = !!(Cons (x, !!(Cons (y, !!Nil))))
    let (!<) x   = !!(Cons (x, !!Nil))

    let rec inj fa l = !! (GT.gmap(t) fa (inj fa) l)
    
    let prj_k fa k l =
      let rec inner l =
        GT.gmap(t) fa inner (prj_k k l)
      in
      inner l

    let prj fa l = prj_k fa (fun _ -> raise Not_a_value) l

    let ground = {
      GT.gcata = ();
      GT.plugins = 
        object(this) 
          method html    fa l = GT.html   (t) fa (this#html    fa) l
          method eq      fa l = GT.eq     (t) fa (this#eq      fa) l
          method compare fa l = GT.compare(t) fa (this#compare fa) l
          method foldr   fa l = GT.foldr  (t) fa (this#foldr   fa) l
          method foldl   fa l = GT.foldl  (t) fa (this#foldl   fa) l
          method gmap    fa l = GT.gmap   (t) fa (this#gmap    fa) l
          method show    fa l = "[" ^
            (GT.transform(t) 
               (GT.lift fa)
               (GT.lift (this#show fa))
               (object inherit ['a,'a ground] @t[show]
                  method c_Nil   _ _      = ""
                  method c_Cons  i s x xs = x.GT.fx () ^ (match xs.GT.x with Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                end) 
               () 
               l
            ) ^ "]"
        end
    }

    let logic = {
      GT.gcata = ();
      GT.plugins = 
        object(this) 
          method html    fa l = GT.html   (t) fa (this#html    fa) l
          method eq      fa l = GT.eq     (t) fa (this#eq      fa) l
          method compare fa l = GT.compare(t) fa (this#compare fa) l
          method foldr   fa l = GT.foldr  (t) fa (this#foldr   fa) l
          method foldl   fa l = GT.foldl  (t) fa (this#foldl   fa) l
          method gmap    fa l = GT.gmap   (t) fa (this#gmap    fa) l
          method show    fa l = 
            GT.show(logic')
              (fun l -> "[" ^            
                 (GT.transform(t) 
                    (GT.lift fa)
                    (GT.lift (this#show fa))
                    (object inherit ['a,'a logic] @t[show]
                       method c_Nil   _ _      = ""
                       method c_Cons  i s x xs = x.GT.fx () ^ (match xs.GT.x with Value Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                     end) 
                    () 
                    l
                 ) ^ "]"
              ) 
              l
        end
    }

  end

let (%)  = List.(%)
let (%<) = List.(%<)
let (!<) = List.(!<)

exception Occurs_check

type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let rec wrap (x : Obj.t) =
  Obj.(
    let is_valid_tag =
      TopList.fold_left
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
	let f = if t = double_array_tag then !!! double_field else field in
	Boxed (t, size x, f x)
      else Invalid t
    )

let generic_show x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner o =
    match wrap o with
    | Invalid n             -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed n when !!!n=0  -> Buffer.add_string b "[]"
    | Unboxed n             -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!!n))
    | Boxed (t,l,f) when t=0 && l=1 && (match wrap (f 0) with Unboxed i when !!!i >=10 -> true | _ -> false) ->
       Printf.bprintf b "var%d" (match wrap (f 0) with Unboxed i -> !!!i | _ -> failwith "shit")

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
  end = 
  struct
    type t = GT.int GT.list * int 

    let empty () = ([0], 10)

    let fresh (a, current) =
      let v = Var (a, current, []) in
      (!!!v, (a, current+1))

    let var_tag, var_size =
      let v = Var ([], 0, []) in
      Obj.tag (!!! v), Obj.size (!!! v)

    let var (a, _) x =
      let t = !!! x in
      if Obj.tag  t = var_tag  &&
         Obj.size t = var_size &&
         (let q = Obj.field t 0 in
          not (Obj.is_int q) && q == (!!!a)
         )
      then let Var (_, i, _) = !!! x in Some i
      else None

  end

module Subst :
  sig
    type t

    val empty   : t

    val of_list : (int * Obj.t * Obj.t) list -> t 
    val split   : t -> Obj.t list * Obj.t list 
    val walk    : Env.t -> 'a logic -> t -> 'a logic
    val unify   : Env.t -> 'a logic -> 'a logic -> t option -> (int * Obj.t * Obj.t) list * t option
    val show    : t -> string
  end =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    type t = (Obj.t * Obj.t) M.t

    let show m = (M.fold (fun i (_, x) s -> s ^ Printf.sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}"

    let empty = M.empty

    let of_list l = TopList.fold_left (fun s (i, v, t) -> M.add i (v, t) s) empty l

    let split s = M.fold (fun _ (x, t) (xs, ts) -> x::xs, t::ts) s ([], []) 

    let rec walk env var subst =
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (snd (M.find i (!!! subst))) subst with Not_found -> var

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
	      else occurs env xi (!!!(f i)) subst || inner (i+1)
	    in
	    inner 0

    let unify env x y subst =
      let rec unify x y (delta, subst) = 
        let extend xi x term delta subst =
          if occurs env xi term subst then raise Occurs_check
          else (xi, !!!x, !!!term)::delta, Some (!!! (M.add xi (!!!x, term) (!!! subst)))
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
		           then inner (i+1) (unify (!!!(fx i)) (!!!(fy i)) (delta, subst))
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
    let show  (env, subst, constr) = Printf.sprintf "st {%s, %s}" (Subst.show subst) (GT.show(GT.list) Subst.show constr)
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
            TopList.fold_left (fun css' cs -> 
              let x, t  = Subst.split cs in
	      try
                let p, s' = Subst.unify env (!!!x) (!!!t) subst' in
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
    let prefix = TopList.split (TopList.map (fun (_, x, t) -> (x, t)) prefix) in
    let subsumes subst (vs, ts) = 
      try 
        match Subst.unify env !!!vs !!!ts (Some subst) with
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

module Fresh =
  struct

    let succ prev f = call_fresh (fun x -> prev (f x))
 
    let zero  f = f 
    let one   f = succ zero f
    let two   f = succ one f
    let three f = succ two f
    let four  f = succ three f
    let five  f = succ four f
 
    let q     = one
    let qr    = two
    let qrs   = three
    let qrst  = four
    let pqrst = five

  end

let rec refine : 'a . State.t -> 'a logic -> 'a logic = fun ((e, s, c) as st) x ->  
  let rec walk' recursive env var subst =
    let var = Subst.walk env var subst in
    match Env.var env var with
    | None ->
        (match wrap (Obj.repr var) with
         | Unboxed _ -> !!!var
         | Boxed (t, s, f) ->
            let var = Obj.dup (Obj.repr var) in
            let sf =
              if t = Obj.double_array_tag
              then !!! Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to s - 1 do
              sf var i (!!!(walk' true env (!!!(f i)) subst))
           done;
           !!!var
         | Invalid n -> invalid_arg (Printf.sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i when recursive ->        
        (match var with
         | Var (a, i, _) -> 
            let cs = 
	      TopList.fold_left 
		(fun acc s -> 
		   match walk' false env (!!!var) s with
		   | Var (_, j, _) when i = j -> acc
		   | t -> (refine st t) :: acc
		)	
		[]
		c
	    in
	    Var (a, i, cs)
        )
    | _ -> var
  in
  walk' true e (!!!x) s

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

type 'a refiner = State.t Stream.t -> 'a logic Stream.t

let refiner : 'a logic -> 'a refiner = fun x ans ->
  Stream.map (fun st -> refine st x) ans

module LogicAdder = 
  struct
    let zero f = f
 
    let succ (prev: 'a -> State.t -> 'b) (f: 'c logic -> 'a) : State.t -> 'c refiner * 'b =
      call_fresh (fun logic st -> (refiner logic, prev (f logic) st))
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

