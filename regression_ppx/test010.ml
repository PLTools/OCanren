(** This is part of HM inferencer demo from noCanren.
  * Look how we are using another extension point which is renaming of 'distrib'
  *)

let () = print_endline "test010"

[%%ocanren_inject type nonrec state = S [@@deriving gt ~options:{ gmap }]]

module _ = struct
  [%%ocanren_inject type nonrec u = U of state [@@deriving gt ~options:{ gmap }]]
end

module Move = struct
  [%%ocanren_inject
  type nonrec 'a move =
    | Forward of 'a
    | Backward of 'a
  [@@deriving gt ~options:{ gmap }]]
end

[%%ocanren_inject type hum_moves = GT.int Move.move [@@deriving gt ~options:{ gmap }]]
[%%ocanren_inject type asdf = GT.int GT.list [@@deriving gt ~options:{ gmap }]]
