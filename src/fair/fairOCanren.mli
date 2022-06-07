(* type 'history call
type 'history stream


module Call : sig
    val one : unit ->
              ((Obj.t list -> 'b) -> 'a -> 'b) *
              (('a -> 'b) -> Obj.t list -> 'b)
    val two : unit ->
              ((Obj.t list -> 'c) -> 'a -> 'b -> 'c) *
              (('a -> 'b -> 'c) -> Obj.t list -> 'c)
    val three : unit ->
              ((Obj.t list -> 'd) -> 'a -> 'b -> 'c -> 'd) *
              (('a -> 'b -> 'c -> 'd) -> Obj.t list -> 'd)

    val create : (unit -> (('a list -> 'history call) -> 'c) * ('c -> ('a list -> unit))) -> string -> 'c -> 'c
  end *)
