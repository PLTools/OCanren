type term
type state
type 'a call
type 'a goal
type 'a stream
type ('a, 'b) strategy
type fair_history

module Call : sig
  val one : unit ->
            ((term list -> 'b) -> 'a -> 'b) *
            (('a -> 'b) -> term list -> 'b)
  val two : unit ->
            ((term list -> 'c) -> 'a -> 'b -> 'c) *
            (('a -> 'b -> 'c) -> term list -> 'c)
  val three : unit ->
            ((term list -> 'd) -> 'a -> 'b -> 'c -> 'd) *
            (('a -> 'b -> 'c -> 'd) -> term list -> 'd)
  val four  : unit ->
            ((term list -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a) *
            (('f -> 'g -> 'h -> 'e -> 'a) -> term list -> 'a)

  val create : (unit -> ((term list -> 'a goal) -> 'b) * ('b -> (term list -> 'a goal))) -> string -> 'b -> 'b
end

module M : sig
  type 'a t

  val of_seq : (string * 'a) Seq.t -> 'a t
end

open Logic

val (|||) : 'a goal -> 'a goal -> 'a goal
val (&&&) : 'a goal -> 'a goal -> 'a goal
val (===) : 'a ilogic -> 'a ilogic -> 'b goal
val call_fresh : ('a -> 'b goal) -> 'b goal
val call : (unit -> ((term list -> 'a goal) -> 'b) * ('b -> term list -> 'a goal)) -> string -> 'b -> 'b
val conde : 'a goal list -> 'a goal
val (?&) : 'a goal list -> 'a goal

val q : unit -> 
  (('a ilogic -> 'b goal) -> state -> 'b -> 'a ilogic * 'b stream) *
  ('c ilogic -> Env.t -> 'c reified) * ('d -> 'd) *
  (('e -> 'f) -> 'e -> 'f)

val qr : unit ->
  (('a ilogic -> 'b ilogic -> 'c goal) ->
   state -> 'c -> 'a ilogic * ('b ilogic * 'c stream)) *
  ('d ilogic * 'e ilogic ->
   Env.t -> 'd reified * 'e reified) *
  ('f * ('g * 'h) -> ('f * 'g) * 'h) *
  (('i -> 'j -> 'k) -> 'i * 'j -> 'k)

val qrs : unit ->
  (('a ilogic -> 'b ilogic -> 'c ilogic -> 'd goal) ->
   state ->
   'd ->
   'a ilogic *
   ('b ilogic * ('c ilogic * 'd stream))) *
  ('e ilogic * ('f ilogic * 'g ilogic) ->
   Env.t -> 'e reified * ('f reified * 'g reified)) *
  ('h * ('i * ('j * 'k)) -> ('h * ('i * 'j)) * 'k) *
  (('l -> 'm -> 'n -> 'o) -> 'l * ('m * 'n) -> 'o)

val run : int ->
  (unit ->
   ('a -> state -> 'b -> 'c) * ('d -> Env.t -> 'e) *
   ('c -> 'f * 'b stream) * ('g -> 'e -> 'h)) ->
  'g -> ('i, 'b) strategy -> 'a -> 'h list

val fair_strategy : bool list M.t -> (state -> term -> int) -> (int list option, fair_history) strategy
val lb_strategy : (unit, unit) strategy

val term_height : state -> term -> int
val term_size : state -> term -> int

val show_steps : 'a goal -> ('b, 'a) strategy -> (state -> term -> int) -> ('a -> string) -> int -> unit
val fair_hprinter : fair_history -> string