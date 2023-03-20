(** This is part of HM inferencer demo from noCanren.
  * Look how we are using another extension point which is renaming of 'distrib'
  *)

let () = print_endline "test010"

[%%ocanren_inject type nonrec state = S [@@deriving gt ~options:{ gmap }]]

module _ = struct
  [%%ocanren_inject type nonrec u = U of state [@@deriving gt ~options:{ gmap }]]
end
