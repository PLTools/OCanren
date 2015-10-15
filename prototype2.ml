type 'a logic = Var of int | Term of 'a

module M = Map.Make (struct type t = int include Pervasives end)

class ['a] subst =
  object (this)
    val m = M.empty 
    method update (x : int) (v : 'a logic) = Some {< m = M.add x v m >} 
    method walk x = 
      let rec find = function 
      | (Var i) as x -> (try find (M.find i m) with Not_found -> x) 
      | expr         -> expr
      in find x
  end

let unify fa x y =
  function 
  | None -> None
  | (Some s) as s' ->
     let x = s#walk x in
     let y = s#walk y in
     match x, y with
     | Var i , Var j when i = j -> s'
     | Var i , _                -> s#update i y
     | _     , Var j            -> s#update j x
     | Term x, Term y           -> fa x y s'

type t = A | B

type t_logic = [`A | `B] logic

let unify_t x y s =
  unify (
    fun x y -> function
    | None -> None
    | (Some s) as s' ->
       match x, y with
       | `A, `A -> s'
       | `B, `B -> s'
       | _      -> None 
  ) x y s

let unify_int (x : int logic) (y : int logic) s =
  unify (
    fun x y -> function
    | None -> None
    | (Some _) as s' ->
       if x = y then s' else None
  ) x y s

let unify_string (x : string logic) (y : string logic) s =
  unify (
    fun x y -> function
    | None -> None
    | (Some _) as s' ->
       if x = y then s' else None
  ) x y s

type p = C of int | D of string

type p_logic = [`C of int logic | `D of string logic] logic

let unify_p x y s =
  unify (
    fun x y -> function
    | None -> None
    | (Some s) as s' ->
        match x, y with
        | `C x, `C y -> unify_int    x y s
	| `D x, `D y -> unify_string x y s
	| _          -> None 
  )
(*
let _ =
  Printf.printf "%s\n"
    (match unify_t (Term `A) (Term `B) (Some (new subst)) with
     | None   -> "fail"
     | Some _ -> "ok"
    )
     
*)
