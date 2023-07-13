(** This test demostrated that we need type annotation near prj_exn/reify
to avoid compilation error about `_weak` type variables
  *)

let () = print_endline "test010"

module JType = struct
  [%%distrib
  type 'targ ground =
    | Array of 'targ ground
    | Var of GT.int
  [@@deriving gt ~options:{ show; fmt; gmap }]]
end

[%%distrib type targ = Typ of targ JType.ground [@@deriving gt ~options:{ show; fmt; gmap }]]
