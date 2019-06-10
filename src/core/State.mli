type t
(*=
  { env   : Env.t
  ; subst : Subst.t
  ; ctrs  : Disequality.t
  ; scope : Var.scope
  }
*)

val empty : unit -> t

val fresh : t -> 'a

val new_scope : t -> t

val unify : 'a -> 'a -> t -> t option
val diseq : 'a -> 'a -> t -> t option

val reify : 'a -> t -> Answer.t list