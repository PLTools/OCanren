
@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

let logic = {logic with
  gcata = ();
  plugins =
    object
      method gmap      = logic.plugins#gmap
      method html      = logic.plugins#html
      method eq        = logic.plugins#eq
      method compare   = logic.plugins#compare
      method foldl     = logic.plugins#foldl
      method foldr     = logic.plugins#foldr
      method show fa x =
        GT.transform(logic)
          (GT.lift fa)
          (object inherit ['a] @logic[show]
             method c_Var _ s i cs =
               let c = match cs with
               | [] -> ""
               | _  -> sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
               in
               sprintf "_.%d%s" i c
             method c_Value _ _ x = x.GT.fx ()
           end)
          ()
          x
    end
}

type ('a, 'b) injected = 'a

external lift : 'a -> ('a, 'a) injected                      = "%identity"
external inj  : ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"

exception Not_a_value

let to_logic x = Value x

let from_logic = function
| Value x    -> x
| Var (_, _) -> raise Not_a_value

let (!!) x = inj (lift x)

let prj x = let rr = make_rr (Env.empty ()) x in rr#prj

class type ['a,'b] reified = object
  method is_open : bool
  method prj     : 'a
  method reify   : (Env.t -> ('a, 'b) injected -> 'b) -> 'b
  method prjc    : (Env.t -> ('a, 'b) injected -> 'a) -> 'a
end

let make_rr : Env.t -> ('a, 'b) injected -> ('a, 'b) reified  = fun env x ->
  object (self)
    method is_open            = Env.is_open env x
    method prj                = if self#is_open then raise Not_a_value else !!!x
    method reify reifier      = reifier env x
    method prjc  onvar        = onvar   env x
  end