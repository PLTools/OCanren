open OCanren

module _ = struct
  type state = (int * int) * (int * int) GT.list [@@deriving reify]
end

let () = print_endline "test005"
