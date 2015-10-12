let rec fix f = f (fix f)
let id x = x

module M = Map.Make (struct type t = int include Pervasives end)

type 'a logic = Variable of int | Term of 'a

class ['a] subst =
  object
    val m : 'a logic M.t = M.empty
    method update (i : int) (x : 'a logic) = {< m = M.add i x m >}
    method walk x =
      fix (fun f -> 
	function
	| Variable i -> (try f (M.find i m) with Not_found -> x)
	| x -> x) x
  end

let unify ua x y = function
| None -> None
| (Some s') as s ->
   let x, y = s'#walk x, s'#walk y in
   match x, y with
   | Variable i, Variable j -> 
       if i = j then s else Some (s'#subst i y)
   | Variable i, _ ->
       Some (s'#subst i y)
   | _, Variable j -> 
       Some (s'#subst j x)
   | _ -> ua x y s
  
