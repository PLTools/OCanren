(** {1 Main}   *)

(** In this module you can find convenience functions to speedup your introduction to OCanre.
    In realy projects, more likely you will need more low-level interface like {!OCanren.run}. *)

(**
  The call [run_r reifier to_string count num num_handler ("description", goal)] should be used
  to a goal [goal], get [count] answers, reify all of them using [reifier], and print them using
  provided [to_string] function. Auxiliary functions [num] and [num_handler] allow using this
  function in polyvariadic manner
  (see {!OCanren.q}, {!OCanren.qr} and {!val-qh}, {!val-qrh} for examples).

  For example:

    {[
    open OCanren
    open Tester
    let () =
      run_r (Std.List.prj_exn prj_exn) (GT.show GT.int) 1
        q qh ("simple", (fun q -> q === inj 1))
    ]}

  OCanren uses many types and various type printing functions. That's why Scheme equivalent is much shorter.

    {@scheme[
      (run 1 (q) (== ,q 1))
      ]}

  In scheme
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

(** {1 Auxiliary functions} *)

val qh : (int -> string -> 'a -> unit) -> 'a -> unit -> unit
val qrh : (int -> string -> 'a -> unit) -> 'a -> 'a -> unit -> unit
val qrsh : (int -> string -> 'a -> unit) -> 'a -> 'a -> 'a -> unit -> unit
val qrsth :
  (int -> string -> 'a -> unit) -> 'a -> 'a -> 'a -> 'a -> unit -> unit
