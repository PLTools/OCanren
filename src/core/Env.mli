type t

val empty         : unit -> t

val create        : anchor:Var.env -> t

val fresh         : scope:Var.scope -> t -> 'a

val check         : t -> Var.t -> bool

val check_exn     : t -> Var.t -> unit

val is_var        : t -> 'a -> bool

val var           : t -> 'a -> Var.t option

val freevars      : t -> 'a -> VarSet.t

val is_open       : t -> 'a -> bool

val equal         : t -> t -> bool