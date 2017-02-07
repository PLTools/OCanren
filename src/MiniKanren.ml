(*
 * MiniKanren: miniKanren implementation.
 * Copyright (C) 2015-2016
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, 
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

module Stream =
  struct

    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec is_empty = function
    | Nil    -> true
    | Lazy s -> is_empty @@ Lazy.force s
    | _ -> false

    let rec retrieve ?(n=(-1)) s =
      if n = 0
      then [], s
      else match s with
           | Nil          -> [], s
           | Cons (x, xs) -> let xs', s' = retrieve ~n:(n-1) xs in x::xs', s'
	   | Lazy  z      -> retrieve ~n:n (Lazy.force z)            

    let take ?(n=(-1)) s = fst @@ retrieve ~n:n s

    let hd s = List.hd @@ take ~n:1 s
    let tl s = snd @@ retrieve ~n:1 s

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
    | Lazy s -> Lazy (Lazy.from_fun (fun () -> map f @@ Lazy.force s))

    let rec iter f = function
    | Nil -> ()
    | Cons (x, xs) -> f x; iter f xs
    | Lazy s -> iter f @@ Lazy.force s

    let rec zip fs gs = match (fs, gs) with
    | Nil, Nil -> Nil
    | Cons (x, xs), Cons (y, ys) -> Cons ((x, y), zip xs ys)
    | _, Lazy s -> Lazy (Lazy.from_fun (fun () -> zip fs (Lazy.force s)))
    | Lazy s, _ -> Lazy (Lazy.from_fun (fun () -> zip (Lazy.force s) gs))
    | Nil, _ -> invalid_arg "streams have different lengths"
    | _, Nil -> invalid_arg "streams have different lengths"

  end

let (!!!) = Obj.magic;;

type 'a logic = 
  | Var of int list * int * 'a logic list 
  | Value of 'a
  [@@deriving visitors { variety = "map"   ; name = "map_logic_t"    }, 
              visitors { variety = "reduce"; name = "reduce_logic_t" }]

let map_logic map x =
  let v = object
    inherit [_] map_logic_t
    method visit_'a env c0 = map c0
  end
  in v#visit_logic () x

let rec show_logic show x = 
  let v = object
    inherit [_] reduce_logic_t
    
    method zero = ""
    method plus = (^)
    
    method visit_Var env c0 i cs = 
      let s = 
        match cs with 
	| [] -> ""
        | _  -> Printf.sprintf " %s" (List.fold_left (fun a c -> a ^ "=/= " ^ (show_logic show c) ^ ";") "" cs)
      in
      Printf.sprintf "_.%d%s" i s
    
    method visit_'a  env c0 = show c0
  end 
  in v#visit_logic () x

type 'a unlogic = [`Var of int * 'a logic list | `Value of 'a]

let destruct = function
| Var (_, i, c) -> `Var (i, c)
| Value x       -> `Value x

exception Not_a_value 

let (!!) x = Value x
let inj = (!!)

let prj_k k = function Value x -> x | Var (_, i, c) -> k i c
let prj x = prj_k (fun _ -> raise Not_a_value) x

let (!?) = prj

exception Occurs_check

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
    | Unboxed n when !!!n=0 -> Buffer.add_string b "[]"
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
    type t = int list * int 

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

    let of_list l = List.fold_left (fun s (i, v, t) -> M.add i (v, t) s) empty l

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
    let show  (env, subst, constr) = 
      let subst'  = Subst.show subst in
      let constr' = List.fold_left (fun a s -> a ^ (Subst.show s) ^ ";") "" constr in
      Printf.sprintf "st {%s, %s}" subst' ("[" ^ constr' ^ "]")
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
    let prefix = List.split (List.map (fun (_, x, t) -> (x, t)) prefix) in
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

let success st = Stream.cons st Stream.nil
let failure _  = Stream.nil
 
let eqo x y t =
  conde [
    (x === y) &&& (t === !!true);
    (x =/= y) &&& (t === !!false);
  ]

let neqo x y t =
  conde [
    (x =/= y) &&& (t === !!true);
    (x === y) &&& (t === !!false);
  ];;

type ('a, 'l) llist = 
  | Nil 
  | Cons of 'a * 'l 
  [@@deriving visitors { variety = "map"   ; name = "map_llist_t"   },
              visitors { variety = "reduce"; name = "reduce_llist_t" }]

let map_llist fa fl x =
  let v = object
    inherit [_] map_llist_t
    method visit_'a env c0 = fa c0
    method visit_'l env c0 = fl c0 
  end
  in v#visit_llist () x

let show_llist fa fl x =
  let v = object
    inherit [_] reduce_llist_t
    method zero = ""
    method plus = (^)
    method visit_'a env c0 = fa c0
    method visit_'l env c0 = fl c0
    method visit_Cons env c0 c1 = (fa c0) ^ "; " ^ (fl c1)
  end
  in "[" ^ v#visit_llist () x ^ "]"

type 'a lnat = 
  | O 
  | S of 'a 
  [@@deriving visitors { variety = "map"   ; name = "map_lnat_t"    },
              visitors { variety = "reduce"; name = "reduce_lnat_t" }]

let map_lnat fa x =
  let v = object
    inherit [_] map_lnat_t
    method visit_'a env c0 = fa c0
  end
  in v#visit_lnat () x

let show_lnat fa x = 
  let v = object
    inherit [_] reduce_lnat_t
    method zero = ""
    method plus = (^)
    method visit_O  env    = "O"
    method visit_S  env c0 = "S(" ^ (fa c0) ^ ")"
    method visit_'a env c0 = fa c0
  end
  in v#visit_lnat () x

module Bool =
  struct

    type 'a logic' = 'a logic 

    type ground = bool

    type logic = bool logic'

    let show_ground b = if b then "true" else "false"
    let show_logic    = show_logic show_ground

    let (!) = (!!)

    let (|^) a b c =
      conde [
        (a === !false) &&& (b === !false) &&& (c === !true);
        (a === !false) &&& (b === !true)  &&& (c === !true);
        (a === !true)  &&& (b === !false) &&& (c === !true);
        (a === !true)  &&& (b === !true)  &&& (c === !false);
      ]

    let noto' a na = (a |^ a) na

    let noto a = noto' a !true

    let oro a b c = 
      Fresh.two (fun aa bb ->      
        ((a  |^ a) aa) &&&
        ((b  |^ b) bb) &&&
        ((aa |^ bb) c)
      )

    let ando a b c = 
      Fresh.one (fun ab ->
        ((a  |^ b) ab) &&&
        ((ab |^ ab) c)
      )

    let (&&) a b = ando a b !true
    let (||) a b = oro  a b !true

  end

module Nat =
  struct

    type 'a logic' = 'a logic

    type 'a t = 'a lnat

    type ground = ground t

    type logic  = logic t logic'

    let show_logic' = show_logic

    let rec show_ground = fun x -> show_lnat show_ground x
    let rec show_logic  = fun x -> show_logic' (show_lnat show_logic) x

    let rec of_int n = if n <= 0 then O else S (of_int (n-1))
    let rec to_int   = function O -> 0 | S n -> 1 + to_int n

    let (!) = (!!)
    
    let rec inj n = ! (map_lnat inj n)

    let prj_k k n =
      let rec inner n =
        map_lnat inner (prj_k k n)
      in
      inner n

    let prj n = prj_k (fun _ -> raise Not_a_value) n

    let rec addo x y z =
      conde [
        (x === !O) &&& (z === y);
        Fresh.two (fun x' z' ->
           (x === !(S x')) &&&
           (z === !(S z')) &&&
           (addo x' y z')
        )
      ]

    let (+) = addo

    let rec mulo x y z =
      conde [
        (x === !O) &&& (z === !O);
        Fresh.two (fun x' z' ->
          (x === !(S x')) &&&
          (addo y z' z) &&&
          (mulo x' y z')
        )
      ]

    let ( * ) = mulo

    let rec leo x y b =
      conde [
        (x === !O) &&& (b === !true);
        (x =/= !O) &&& (y === !O) &&& (b === !false);
        Fresh.two (fun x' y' ->	
          conde [
            (x === !(S x')) &&& (y === !(S y')) &&& (leo x' y' b)           
          ]
        )        
      ]

    let geo x y b = leo y x b

    let (<=) x y = leo x y !true
    let (>=) x y = geo x y !true

    let gto x y b = conde [(x >= y) &&& (x =/= y) &&& (b === !true)]
    let lto x y b = gto y x b

    let (>) x y = gto x y !true
    let (<) x y = lto x y !true
    
  end

let rec inj_nat n = 
  if n <= 0 then inj O
  else inj (S (inj_nat @@ n-1))

let rec prj_nat n = 
  match prj n with
  | O   -> 0
  | S n -> 1 + prj_nat n

module List =
  struct

    include List

    type 'a logic' = 'a logic

    type ('a, 'l) t = ('a, 'l) llist

    type 'a ground = ('a, 'a ground) t
    type 'a logic  = ('a, 'a logic)  t logic'

    let show_logic' = show_logic

    let rec show_ground fa = show_llist fa (show_ground fa)
    let rec show_logic  fa = show_logic' (show_llist fa (show_logic fa))

    let rec of_list = function [] -> Nil | x::xs -> Cons (x, of_list xs)
    let rec to_list = function Nil -> [] | Cons (x, xs) -> x::to_list xs

    let (%)  x y = !!(Cons (x, y))
    let (%<) x y = !!(Cons (x, !!(Cons (y, !!Nil))))
    let (!<) x   = !!(Cons (x, !!Nil))

    let nil = inj Nil

    let rec inj fa l = !! (map_llist fa (inj fa) l)
    
    let prj_k fa k l =
      let rec inner l =
        map_llist fa inner (prj_k k l)
      in
      inner l

    let prj fa l = prj_k fa (fun _ -> raise Not_a_value) l

    let (!) = (!!)

    let rec foldro f a xs r =
      conde [
        (xs === !Nil) &&& (a === r);
        Fresh.three (
          fun h t a'->
            (xs === h % t) &&&
            (f h a' r) &&&
            (foldro f a t a')
        )
      ]

    let rec mapo f xs ys =
      conde [
        (xs === !Nil) &&& (ys === !Nil);
        Fresh.two (
          fun z zs ->
            (xs === z % zs) &&&
            (Fresh.two (
               fun a1 a2 ->
                 (f z a1) &&&
                 (mapo f zs a2) &&&
                 (ys === a1 % a2)
            ))
        )
      ]

    let filtero p xs ys =
      let folder x a a' =
        conde [
          (p x !true) &&& (x % a === a');
          (p x !false) &&& (a === a')
        ]
      in
      foldro folder !Nil xs ys

    let rec lookupo p xs mx =
      conde [
        (xs === !Nil) &&& (mx === !None);
        Fresh.two (
          fun h t ->
             (h % t === xs) &&&
             (conde [
                (p h !true) &&& (mx === !(Some h));
                (p h !false) &&& (lookupo p t mx)
             ])
        )
      ]

    let anyo = foldro Bool.oro !false

    let allo = foldro Bool.ando !true

    let rec lengtho l n =
      conde [
        (l === !Nil) &&& (n === !O);
        Fresh.three (fun x xs n' ->
          (l === x % xs)  &&& 
          (n === !(S n')) &&&
          (lengtho xs n')
        )
      ]
	
    let rec appendo a b ab =
      conde [
        (a === !Nil) &&& (b === ab);
        Fresh.three (fun h t ab' ->
  	  (a === h%t) &&&
	  (h%ab' === ab) &&&
	  (appendo t b ab') 
        )   
      ]
  
    let rec reverso a b = 
      conde [
        (a === !Nil) &&& (b === !Nil);
        Fresh.three (fun h t a' ->
	  (a === h%t) &&&
	  (appendo a' !<h b) &&&
	  (reverso t a')
        )
      ]

    let rec membero l a =
      Fresh.two (fun x xs ->
        (l === x % xs) &&&
        (conde [
           x === a;
           (x =/= a) &&& (membero xs a)
         ])
      )
  end

let rec inj_list = function
| []    -> inj Nil
| x::xs -> inj (Cons (inj x, inj_list xs))

let rec prj_list l =
  match prj l with
  | Nil -> []
  | Cons (x, xs) -> prj x :: prj_list xs

let (%)  = List.(%)
let (%<) = List.(%<)
let (!<) = List.(!<)
let nil  = List.nil

let rec inj_nat_list = function
| []    -> !!Nil
| x::xs -> inj_nat x % inj_nat_list xs

let rec prj_nat_list l =
  match prj l with
  | Nil -> []
  | Cons (x, xs) -> prj_nat x :: prj_nat_list xs

let rec refine : State.t -> 'a logic -> 'a logic = fun ((e, s, c) as st) x ->  
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
	      List.fold_left 
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

