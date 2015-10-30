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
      LOG[trace1] (logn "mplus");
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

let (!!) = Obj.magic

type var = Var of int
type w   = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

type config =
  { mutable do_log: bool;
    mutable do_readline: bool }

let config = { do_log=true; do_readline=false }

let () =
  let args = ref [("-r", Arg.Unit (fun () -> config.do_readline <- true), "readlines")] in
  LOG[trace1](
       args := ("-q", Arg.Unit (fun () -> config.do_log <- false), "quite") :: !args;
       args := ("-v", Arg.Unit (fun () -> config.do_log <- true), "verbose") :: !args );
  Arg.parse !args
            (fun s -> Printf.eprintf "Unknown parameter '%s'\n" s; exit 0)
            "This is usage message"

let logn fmt =
  if config.do_log then Printf.kprintf (Printf.printf "%s\n%!") fmt
  else Printf.kprintf (fun fmt -> ignore (Printf.sprintf "%s" fmt)) fmt

let logf fmt =
  if config.do_log then Printf.kprintf (Printf.printf "%s%!") fmt
  else Printf.kprintf (fun fmt -> ignore (Printf.sprintf "%s" fmt)) fmt

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
    val fresh  : t -> 'a * t
    val var    : t -> 'a -> int option
    val vars   : t -> var list
    val show   : t -> string
  end = 
  struct
    module H = Hashtbl.Make (
      struct
        type t = var
        let hash = Hashtbl.hash
        let equal = (==)
      end)

    type t = unit H.t * int

    let counter_start = 10 (* 1 to be able to detect empty list *)
    let empty () = (H.create 1024, counter_start)

    let fresh (h, current) =
      LOG[trace1] (logn "fresh var %d" current);
      if config.do_readline then ignore (read_line ());
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
    val walk  : Env.t -> 'a -> t -> 'a
    val walk' : Env.t -> 'a -> t -> 'a
    val unify : Env.t -> 'a -> 'a -> t option -> t option
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
	   | Unboxed _ -> var
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
	   with Not_found -> var
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

let show_var : State.t -> 'a -> (unit -> string) -> 'string = fun (e, _) x k ->
  match Env.var e x with
  | Some i -> Printf.sprintf "_.%d" i
  | None   -> k ()

type    int       = GT.int
type    string    = GT.string
type 'a list      = 'a GT.list
type    bool      = GT.bool
type    char      = GT.char
type    unit      = GT.unit
type    int32     = GT.int32
type    int64     = GT.int64
type    nativeint = GT.nativeint

class mkshow_string_t =
  object
    method t_string env str = show_var env str (fun _ -> str)
  end

class mkshow_int_t =
  object
    method t_int env int = show_var env int (fun _ -> string_of_int int)
  end

class mkshow_bool_t =
  object
    method t_bool env bool = show_var env bool (fun _ -> string_of_bool bool)
  end

class mkshow_char_t =
  object
    method t_char env char = show_var env char (fun _ -> String.make 1 char)
  end

class mkshow_unit_t =
  object
    method t_unit env (unit : unit) = show_var env unit (fun _ -> "()")
  end

class mkshow_int32_t =
  object
    method t_int32 env int32 = show_var env int32 (fun _ -> Int32.to_string int32)
  end

class mkshow_int64_t =
  object
    method t_int64 env int64 = show_var env int64 (fun _ -> Int64.to_string int64)
  end

class mkshow_nativeint_t =
  object
    method t_nativeint env nativeint = show_var env nativeint (fun _ -> Nativeint.to_string nativeint)
  end

class ['a] mkshow_list_t =
  object
    inherit ['a, State.t, string, State.t, string] @GT.list
    method c_Nil  e s      = show_var e s.GT.x (fun _ -> "[]")
    method c_Cons e s x xs =
      show_var e x.GT.x  (fun _ -> x.GT.fx e) ^ ", " ^
      show_var e xs.GT.x (fun _ -> xs.GT.fx e)
  end

class ['a] mkshow_option_t =
  object
    inherit ['a, State.t, string, State.t, string] @GT.option
    method c_None e s   = show_var e s.GT.x (fun _ -> "None")
    method c_Some e s x = show_var e s.GT.x (fun _ -> "Some (" ^ x.GT.fx e ^ ")")
  end

let mkshow t = t.GT.plugins#mkshow

let int = {GT.gcata = GT.int.GT.gcata;
           GT.plugins = 
             object
               method show    = GT.int.GT.plugins#show
               method html    = GT.int.GT.plugins#html
               method compare = GT.int.GT.plugins#compare
               method eq      = GT.int.GT.plugins#eq
               method map     = GT.int.GT.plugins#map
               method foldl   = GT.int.GT.plugins#foldl
               method foldr   = GT.int.GT.plugins#foldr
               method mkshow  = (fun e x -> show_var e x (fun _ -> GT.transform(GT.int) (new mkshow_int_t) e x))
             end
          }

let string = {GT.gcata = GT.string.GT.gcata;
              GT.plugins = 
                object
                  method show    = GT.string.GT.plugins#show
                  method html    = GT.string.GT.plugins#html
                  method compare = GT.string.GT.plugins#compare
                  method eq      = GT.string.GT.plugins#eq
                  method map     = GT.string.GT.plugins#map
                  method foldl   = GT.string.GT.plugins#foldl
                  method foldr   = GT.string.GT.plugins#foldr
                  method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.string) (new mkshow_string_t) e s))
                end
             }

let bool = {GT.gcata = GT.string.GT.gcata;
            GT.plugins = 
              object
                method show    = GT.bool.GT.plugins#show
                method html    = GT.bool.GT.plugins#html
                method compare = GT.bool.GT.plugins#compare
                method eq      = GT.bool.GT.plugins#eq
                method map     = GT.bool.GT.plugins#map
                method foldl   = GT.bool.GT.plugins#foldl
                method foldr   = GT.bool.GT.plugins#foldr
                method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.bool) (new mkshow_bool_t) e s))
              end
           }

let char = {GT.gcata = GT.char.GT.gcata;
            GT.plugins = 
              object
                method show    = GT.char.GT.plugins#show
                method html    = GT.char.GT.plugins#html
                method compare = GT.char.GT.plugins#compare
                method eq      = GT.char.GT.plugins#eq
                method map     = GT.char.GT.plugins#map
                method foldl   = GT.char.GT.plugins#foldl
                method foldr   = GT.char.GT.plugins#foldr
                method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.char) (new mkshow_char_t) e s))
              end
           }

let unit = {GT.gcata = GT.unit.GT.gcata;
            GT.plugins = 
              object
                method show    = GT.unit.GT.plugins#show
                method html    = GT.unit.GT.plugins#html
                method compare = GT.unit.GT.plugins#compare
                method eq      = GT.unit.GT.plugins#eq
                method map     = GT.unit.GT.plugins#map
                method foldl   = GT.unit.GT.plugins#foldl
                method foldr   = GT.unit.GT.plugins#foldr
                method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.unit) (new mkshow_unit_t) e s))
              end
           }

let int32 = {GT.gcata = GT.int32.GT.gcata;
             GT.plugins = 
               object
                 method show    = GT.int32.GT.plugins#show
                 method html    = GT.int32.GT.plugins#html
                 method compare = GT.int32.GT.plugins#compare
                 method eq      = GT.int32.GT.plugins#eq
                 method map     = GT.int32.GT.plugins#map
                 method foldl   = GT.int32.GT.plugins#foldl
                 method foldr   = GT.int32.GT.plugins#foldr
                 method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.int32) (new mkshow_int32_t) e s))
               end
            }

let int64 = {GT.gcata = GT.int64.GT.gcata;
             GT.plugins = 
               object
                 method show    = GT.int64.GT.plugins#show
                 method html    = GT.int64.GT.plugins#html
                 method compare = GT.int64.GT.plugins#compare
                 method eq      = GT.int64.GT.plugins#eq
                 method map     = GT.int64.GT.plugins#map
                 method foldl   = GT.int64.GT.plugins#foldl
                 method foldr   = GT.int64.GT.plugins#foldr
                 method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.int64) (new mkshow_int64_t) e s))
               end
            }

let nativeint = {GT.gcata = GT.nativeint.GT.gcata;
                 GT.plugins = 
                   object
                     method show    = GT.nativeint.GT.plugins#show
                     method html    = GT.nativeint.GT.plugins#html
                     method compare = GT.nativeint.GT.plugins#compare
                     method eq      = GT.nativeint.GT.plugins#eq
                     method map     = GT.nativeint.GT.plugins#map
                     method foldl   = GT.nativeint.GT.plugins#foldl
                     method foldr   = GT.nativeint.GT.plugins#foldr
                     method mkshow  = (fun e s -> show_var e s (fun _ -> GT.transform(GT.nativeint) (new mkshow_nativeint_t) e s))
                   end
                }

let list = {GT.gcata = GT.list.GT.gcata;
            GT.plugins = 
              object
                method show    = GT.list.GT.plugins#show
                method html    = GT.list.GT.plugins#html
                method compare = GT.list.GT.plugins#compare
                method eq      = GT.list.GT.plugins#eq
                method map     = GT.list.GT.plugins#map
                method foldl   = GT.list.GT.plugins#foldl
                method foldr   = GT.list.GT.plugins#foldr
                method mkshow  = (fun fa e s -> show_var e s (fun _ -> GT.transform(GT.list) fa (new mkshow_list_t) e s))
              end
           }

let option = {GT.gcata = GT.option.GT.gcata;
              GT.plugins = 
                object
                  method show    = GT.option.GT.plugins#show
                  method html    = GT.option.GT.plugins#html
                  method compare = GT.option.GT.plugins#compare
                  method eq      = GT.option.GT.plugins#eq
                  method map     = GT.option.GT.plugins#map
                  method foldl   = GT.option.GT.plugins#foldl
                  method foldr   = GT.option.GT.plugins#foldr
                  method mkshow  = (fun fa e s -> show_var e s (fun _ -> GT.transform(GT.option) fa (new mkshow_option_t) e s))
                end
             }

let call_fresh f (env, subst) =
  let x, env' = Env.fresh env in
  f x (env', subst)

let (===) x y (env, subst) =
  LOG[trace1] (logf "unify '%s' and '%s' in '%s' = " (generic_show !!x) (generic_show !!y) (State.show (env, subst)));
  match Subst.unify env x y (Some subst) with
  | None   -> Stream.nil
  | Some s -> LOG[trace1] (logn "'%s'" (State.show (env, s))); Stream.cons (env, s) Stream.nil

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

let refine (e, s) x = Subst.walk' e x s

let take = Stream.take
