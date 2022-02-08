(** {1 Auxiliary functions} *)

val qh : (int -> string -> 'a -> unit) -> 'a -> unit -> unit
val qrh : (int -> string -> 'a -> unit) -> 'a -> 'a -> unit -> unit
val qrsh : (int -> string -> 'a -> unit) -> 'a -> 'a -> 'a -> unit -> unit
val qrsth :
  (int -> string -> 'a -> unit) -> 'a -> 'a -> 'a -> 'a -> unit -> unit

(** {1 Main} *)

(**
  The call [run_r reifier to_string count num num_handler ("description", goal)] should be used
  to a goal [goal], get [count] answers, reify all of them using [reifier], and print them using
  provided [to_string] function. Auxiliary functions [num] and [num_handler] allow using this
  function in polyvariadic manner

  For example:

    {[
    open OCanren
    run_r (Std.List.prj_exn prj_exn) show_int 1 q qh ("simple", (fun q -> q === inj 1))
    ]}
*)
val run_r :
  ('a OCanren.ilogic, 'b) OCanren.Reifier.t ->
  ('b -> string) ->
  int ->
  (unit ->
   ('c -> OCanren.State.t -> 'd) * ('e -> OCanren.Env.t -> 'f) *
   ('d -> 'e * OCanren.State.t OCanren.Stream.t) * ('g -> 'f -> unit -> unit)) ->
  ((int -> string -> 'a OCanren.reified -> unit) -> 'g) ->
  string * 'c -> unit

(** More general combinator. The {! run_r} is implemented using it. Unlikely will be used in practice
  *)
val run_gen :
  'a ->
  int ->
  (unit ->
   ('b -> OCanren.State.t -> 'c) * ('d -> OCanren.Env.t -> 'e) *
   ('c -> 'd * OCanren.State.t OCanren.Stream.t) * ('f -> 'e -> unit -> unit)) ->
  ('a -> 'f) -> string * 'b -> unit
