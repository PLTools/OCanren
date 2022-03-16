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

let () = print_endline "test006"
