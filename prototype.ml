type 'a logic = Logic_var of int | Logic_expr of 'a

module M = Map.Make (struct type t = int include Pervasives end)

class ['a] subst =
  object (this)
    val m = M.empty 
    method update (x : int) (v : 'a logic) = {< m = M.add x v m >} 
    method walk   (x : 'a logic) = 
      let rec find = function 
      | (Logic_var i) as x -> (try find (M.find i m) with Not_found -> x) 
      | expr               -> expr
      in find x
  end

let unify_logic unify_a x y =
  function 
  | None           -> None
  | (Some s') as s ->
      let x = s'#walk x in
      let y = s'#walk y in
      match x, y with 
      | Logic_var x , (Logic_var  y as y') -> if x = y then s else Some (s'#update x y')
      | _           ,  Logic_var  y        -> Some (s'#update y x)
      | Logic_var x ,  _                   -> Some (s'#update x y)
      | Logic_expr x,  Logic_expr y        -> unify_a x y s 

let rec unify_primitive x y s = 
  unify_logic (fun x y s -> if x = y then s else None) x y s

let unify_string (x : string logic) (y : string logic) = 
  function None -> None | Some s -> s#update_string (unify_primitive x y s#string)

let unify_int (x : int logic) (y : int logic) = 
  function None -> None | Some s -> s#update_int (unify_primitive x y s#int)

type t = [ `A of int | `B of string ]

type t_logic = [`A of int logic | `B of string logic] logic 

let rec unify_t x y = 
  function 
  | None -> None
  | Some s ->
      s#update_t ( 
        unify_logic 
          (fun x y s ->
             match x, y with
             | `A x, `A y -> unify_int    x y s
             | `B x, `B y -> unify_string x y s
             | _          -> None
          )
          x y s#t
      ) 
    
type lambda = Lam of string * lambda | App of lambda * lambda | Var of string

type logic_lambda = [ 
                      `Lam of string logic * logic_lambda logic 
                    | `App of logic_lambda logic * logic_lambda logic 
                    | `Var of string logic
                    ]

let rec unify_lambda x y =
  function
  | None   -> None
  | Some s ->
      s#update_lambda (
        unify_logic (
          fun x y s ->
            match x, y with
            | `Lam (x1, b1), `Lam (x2, b2) -> unify_lambda b1 b2 (unify_string x1 x2 s)                        
            | `App (f1, a1), `App (f2, a2) -> unify_lambda f1 f2 (unify_lambda a1 a2 s) 
            | `Var  x1     , `Var  x2      -> unify_string x1 x2 s
            | _                            -> None  
        )
        x y s#lambda)

type 'a list = Nil | Cons of 'a * 'a list

type 'a logic_list = [`Nil | `Cons of 'a logic * 'a logic_list]

let rec unify_list unify_a x y =
  function
  | None -> None
  | Some s -> 
      s#update_list (
        unify_logic (
          fun x y s ->
          match x, y with
          | `Nil          , `Nil           -> s
          | `Cons (h1, t1), `Cons (h2, t2) -> None (*unify_list unify_a t1 t2 (unify_a h1 h2 s)*)  
          | _                              -> None
        ) x y s#list)

let _ =
  unify_int 
    (Logic_var  1) 
    (Logic_expr 1) 
    (Some 
       (object
          method int =
  object (this)
    val m = M.empty 
    method update (x : int) (v : 'a logic) = {< m = M.add x v m >} 
    method walk   (x : 'a logic) = 
      let rec find = function 
      | (Logic_var i) as x -> (try find (M.find i m) with Not_found -> x) 
      | expr               -> expr
      in find x
  end
(*  
          object
              inherit [int] subst
            end 
*)
          method update_int = invalid_arg ""
        end))

(*
let _ =
  unify_list (unify_logic unify_int) 
    (Logic_var  0 ) 
    (Logic_expr `Nil) 
    (Some 
      (object
         method list =
           object
             inherit [int logic_list] subst
             method int        = invalid_arg ""
             method update_int = invalid_arg ""
           end
         method update_list = invalid_arg ""        
       end))
*)

