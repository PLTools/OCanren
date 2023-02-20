(** Camlp5 syntax extension for {!OCanren}. *)

(** Documentation: TODO *)

(** This function is not really requried. By some reason  compilation crashed without it.
    An issue in pa_ppx?

    https://github.com/chetmurthy/pa_ppx/issues/3
    *)
val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
