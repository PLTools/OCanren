type 'a t

val return: 'a -> 'a t
val run: 'a t -> string list * 'a
val (<*>): ('a -> 'b) t -> 'a t -> 'b t
val (>>|): 'a t -> ('a -> 'b) -> 'b t 
val write: 'a t -> string -> 'a t
(*
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
val foldlm : ('a -> 'b -> 'a t) -> 'a t -> 'b list t -> 'a t
val foldrm : ('b -> 'a -> 'a t) -> 'b list t -> 'a t -> 'a t
val mapm : ('a -> 'b t) -> 'a list -> 'b list t
val fold_right1m_exn: ('a -> 'a -> 'a t) -> 'a list t -> 'a t 
*)
