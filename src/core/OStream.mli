(** Stream type *)
type 'a t

(** Constructors *)

val nil : 'a t

val single : 'a -> 'a t

val cons : 'a -> 'a t -> 'a t

val from_fun : (unit -> 'a t) -> 'a t

val suspend : is_ready:(unit -> bool) -> (unit -> 'a t) -> 'a t

val of_list : 'a list -> 'a t

(** Emptiness test *)
val is_empty : 'a t -> bool

(** [map f s] maps function [f] over the stream [s] *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [iter f s] iterates function [f] over the stream [s] *)
val iter : ('a -> unit) -> 'a t -> unit

(** [filter p s] filters the stream [s] using the predicate [p]
  *   (leaves only those elements [x], for which [p x = true])
  *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** [fold f a s] left-fold over a stream [s]
  *   with function [f] and initial accumulator value [a] *)
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** [zip s s'] zips streams [s] and [s'] into a stream of pairs;
  *   fails with the [Invalid_argument] for the streams of different lengths
  *)
val zip : 'a t -> 'b t -> ('a * 'b) t

(** [mplus s s'] monadic-alternative for streams;
  *   concatenates two streams, the resulting stream contains elements
  *   of both input streams in an interleaved order
  *)
val mplus : 'a t -> 'a t -> 'a t

(** [bind s f] monadic-bind for streams;
  *   maps function [f] over values of the stream [s],
  *    obtaining a stream of streams ['b t t], and then flattens this stream
  *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [retrieve ~n:n s] returns the list of [n]-first elements of [s] and the rest of the stream *)
val retrieve : ?n:int -> 'a t -> 'a list * 'a t

(** [take ~n:n s] returns the list of [n]-first elements of [s] *)
val take : ?n:int -> 'a t -> 'a list

(** [hd s] gets a head of the stream *)
val hd : 'a t -> 'a

(** [tl s] gets a tail of the stream *)
val tl : 'a t -> 'a t