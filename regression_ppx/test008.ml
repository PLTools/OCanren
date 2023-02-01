open OCanren

let () = print_endline "test008"

(* module _ = struct
  [%%distrib
  type 'a ground = (* 'a * *) GT.string * 'a GT.list
  [@@deriving gt ~options:{ show; gmap }]]
end *)

module _ = struct
  [%%distrib type 'a xxx = Cons of 'a * 'a xxx [@@deriving gt ~options:{ show; gmap }]]
end
