open OCanren

module _ = struct
  type state = (int * int) * (int * int) GT.list [@@deriving reify]
end

module _ = struct
  [%%distrib type state = (int * int * int) GT.list]
end

let () = print_endline "test005"
