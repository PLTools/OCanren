(* [Answer.t] - a type that represents (untyped) answer to a query *)
type t

(* [make env t] creates the answer from the environment and term (with constrainted variables)  *)
val make : Env.t -> Term.t -> t

(* [lift env a] lifts the answer into different environment, replacing all variables consistently *)
val lift : Env.t -> t -> t

(* [env a] returns an environment of the answer *)
val env : t -> Env.t

(* [unctr_term a] returns a term with unconstrained variables *)
val unctr_term : t -> Term.t

(* [ctr_term a] returns a term with constrained variables *)
val ctr_term : t -> Term.t

(* [disequality a] returns all disequality constraints on variables in term as a list of bindings *)
val disequality : t -> Binding.t list

(* [equal t t'] syntactic equivalence (not an alpha-equivalence) *)
val equal : t -> t -> bool

(* [hash t] hashing that is consistent with syntactic equivalence *)
val hash : t -> int