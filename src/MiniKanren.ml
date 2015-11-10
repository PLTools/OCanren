module Stream =
  struct

    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.lazy_from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec take ?(n=(-1)) s =
      if n = 0
      then []
      else match s with
           | Nil          -> []
           | Cons (x, xs) -> x :: take ~n:(n-1) xs
	   | Lazy  z      -> take ~n:n (Lazy.force z)            

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

  end

let (!!) = Obj.magic;;

@type 'a logic = Var of GT.int | Value of 'a with show, html, eq, compare, foldl, foldr, map

let logic = {
  logic with plugins = 
    object 
      method html    = logic.plugins#html
      method eq      = logic.plugins#eq
      method compare = logic.plugins#compare
      method foldr   = logic.plugins#foldr
      method foldl   = logic.plugins#foldl
      method map     = logic.plugins#map    
      method show fa x = 
        GT.transform(logic) 
           (GT.lift fa) 
           (object inherit ['a] @logic[show]              
              method c_Var   _ _ i = Printf.sprintf "_.%d" i
              method c_Value _ _ x = x.GT.fx ()
            end) 
           () 
           x
    end
};;

@type 'a llist = Nil | Cons of 'a logic * 'a llist logic with show, html, eq, compare, foldl, foldr, map

let (!) x = Value x

let (%)  x y = !(Cons (x, y))
let (%<) x y = !(Cons (x, !(Cons (y, !Nil))))
let (!<) x   = !(Cons (x, !Nil))

let rec of_list = function
| [] -> !Nil
| x::xs -> !x % (of_list xs)

exception Not_a_value 

let rec to_listk k = function
| Value Nil -> []
| Value (Cons (Value x, xs)) -> x :: to_listk k xs
| z -> k z

let to_list l = to_listk (fun _ -> raise Not_a_value) l


let llist = {
  llist with plugins = 
    object 
      method html    = llist.plugins#html
      method eq      = llist.plugins#eq
      method compare = llist.plugins#compare
      method foldr   = llist.plugins#foldr
      method foldl   = llist.plugins#foldl
      method map     = llist.plugins#map    
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
      let v = Var current in
      H.add h v ();
      (!!v, (h, current+1))

    let var (h, _) x =
      if H.mem h (!! x)
      then let Var i = !! x in Some i
      else None

    let vars (h, _) = H.fold (fun v _ acc -> v :: acc) h []

    let show env = (List.fold_left (fun acc (Var i) -> acc ^ (Printf.sprintf "$%d; " i)) "env {" (vars env)) ^ "}"

  end

module Subst :
  sig
    type t

    val empty : t
    val walk  : Env.t -> 'a logic -> t -> 'a logic
    val walk' : Env.t -> 'a logic -> t -> 'a logic
    val unify : Env.t -> 'a logic -> 'a logic -> t option -> t option
    val show  : t -> string
  end =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    type t = Obj.t M.t

    let show m = (M.fold (fun i x s -> s ^ Printf.sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}"

    let empty = M.empty

    let rec walk env var subst =
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (M.find i (!! subst)) subst with Not_found -> var

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
	  (try walk' env (M.find i (!! subst)) subst
	   with Not_found -> !!var
	  )

    let rec unify env x y = function
    | None -> None
    | (Some subst) as s ->
        let x, y = walk env x subst, walk env y subst in
        match Env.var env x, Env.var env y with
	| Some xi, Some yi -> if xi = yi then s else Some (!! (M.add xi y (!! subst)))
	| Some xi, _       -> Some (!! (M.add xi y (!! subst)))
	| _      , Some yi -> Some (!! (M.add yi x (!! subst)))
	| _ ->
	    let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
            (match wx, wy with
             | Unboxed vx, Unboxed vy -> if vx = vy then s else None
             | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                if tx = ty && sx = sy
		then
		  let rec inner i = function
                  | None -> None
                  | (Some _) as s ->
	               if i < sx
		       then inner (i+1) (unify env (!!(fx i)) (!!(fy i)) s)
		       else s
                  in
		  inner 0 s
                else None
	     | Invalid n, _
             | _, Invalid n -> invalid_arg (Printf.sprintf "Invalid values for unification (%d)" n)
	     | _ -> None
	    )
  end

module State =
  struct  
    type t = Env.t * Subst.t
    let empty () = (Env.empty (), Subst.empty)
    let env = fst
    let show (env, subst) = Printf.sprintf "st {%s, %s}" (Env.show env) (Subst.show subst)
  end

type goal = State.t -> State.t Stream.t

let call_fresh f (env, subst) =
  let x, env' = Env.fresh env in
  f x (env', subst)

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

let (===) x y (env, subst) =
  match Subst.unify env x y (Some subst) with
  | None   -> Stream.nil
  | Some s -> Stream.cons (env, s) Stream.nil

let (=/=) x y ((env, subst) as st) =
  match Subst.unify env x y (Some subst) with
  | None   -> Stream.cons st Stream.nil
  | Some _ -> Stream.nil 

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
 
let run f = f (State.empty ())

let refine (e, s) x = Subst.walk' e (!!x) s

let take = Stream.take
