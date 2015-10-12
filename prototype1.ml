module M = Map.Make (struct type t = int include Pervasives end)

class ['a] subst =
  object (this)
    val m = M.empty 
    method update (x : int) (v : 'a) = Some {< m = M.add x v m >} 
    method walk x = 
      let rec find = function 
      | (`Var i) as x -> (try find (M.find i m) with Not_found -> x) 
      | expr          -> expr
      in find x
  end

let unify fa x y s =
  let x = s#walk x in
  let y = s#walk y in
  match x, y with
  | `Var i, `Var j when i <> j -> s#update i y
  | `Var i, `Var j -> Some s
  | `Var i,  _     -> s#update i y
  |  _    , `Var j -> s#update j x
  |  _             -> fa x y s 

type t = [`A | `B]

let unify_t x y s =
  unify (
    fun x y s ->
      match x, y with
      | `A, `A -> Some s
      | `B, `B -> Some s
      | _      -> None 
  ) x y s

type rect = [`X | `S of rect]

let rec unify_rect x y s =
  unify 
    (fun x y s ->
       match x, y with
       | `X, `X     -> Some s
       | `S x, `S y -> unify_rect x y s
       |  _         -> None
    ) 
    x y s

let unify_int x y s =
  unify 
    (fun x y s ->
       match x, y with
       | `int x, `int y -> if x = y then Some s else None 
       | _ -> None 
    )
    x y s

let unify_string x y s =
  unify 
    (fun x y s ->
       match x, y with
       | `string x, `string y -> if x = y then Some s else None 
       | _ -> None 
    )
    x y s

type argt = [`A of int | `B of string]

let unify_argt x y s =
  unify 
    (fun x y s ->
       match x, y with
       | `A x, `A y -> unify_int    x y s
       | `B x, `B y -> unify_string x y s
       | _          -> None
    )
    x y s

let _ =
  Printf.printf "%s\n"
    (match unify_argt (`A (`A (`int `B))) (`A (`Var 1)) (new subst) with
     | Some _ -> "ok"
     | None   -> "fail"
    )
