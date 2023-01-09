open OCanren

module Gterm = struct
  [%%distrib
  type nonrec ('s, 'xs) t =
    | Symb of 's
    | Seq of 'xs
  [@@deriving gt ~options:{ gmap }]

  type ground = (GT.string, ground Std.List.ground) t]
end

module Gresult = struct
  [%%distrib
  type nonrec ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val of 't
  [@@deriving gt ~options:{ gmap }]

  type ground =
    (GT.string, Gterm.ground, (GT.string, ground) Std.Pair.ground Std.List.ground) t]
end

(* Constructor arguments as a record *)
module _ = struct
  [%%distrib
  type nonrec t =
    | T of
        { x : GT.int
        ; y : GT.int
        }
  [@@deriving gt ~options:{ gmap }]

  type nonrec ground = t]
end

(* records *)
module _ = struct
  [%%distrib
  type nonrec t =
    { x : GT.int
    ; y : GT.int
    }
  [@@deriving gt ~options:{ gmap }]

  type nonrec ground = t]
end

let () = print_endline "test006"

module _ = struct
  [%%distrib
  type ground =
    | Symb of GT.string
    | Seq of ground Std.List.ground
  [@@deriving gt ~options:{ gmap }]]
end
