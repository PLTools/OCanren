(** This is part of HM inferencer demo from noCanren.
  * Look how we are using another extension point which is renaming of 'distrib'
  *)

let () = print_endline "test009"

module _ : sig
  [%%ocanren_inject
  type nonrec ('a1, 'a0) t =
    | LInt of 'a1
    | LBool of 'a0
  [@@deriving gt ~options:{ gmap }]

  type nonrec ground = (GT.int, GT.bool) t]
end = struct
  [%%ocanren_inject
  type nonrec ('a1, 'a0) t =
    | LInt of 'a1
    | LBool of 'a0
  [@@deriving gt ~options:{ gmap }]

  type nonrec ground = (GT.int, GT.bool) t]
end
