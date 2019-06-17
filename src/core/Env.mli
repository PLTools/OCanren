type t

val empty         : unit -> t

val create        : anchor:Term.Var.env -> t

val fresh         : scope:Term.Var.scope -> t -> 'a

val check         : t -> Term.Var.t -> bool

val check_exn     : t -> Term.Var.t -> unit

val is_var        : t -> 'a -> bool

val var           : t -> 'a -> Term.Var.t option

val freevars      : t -> 'a -> Term.VarSet.t

val is_open       : t -> 'a -> bool

val equal         : t -> t -> bool