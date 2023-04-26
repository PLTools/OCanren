let () = print_endline "test014"

module _ = struct
  [%%ocanren_inject
  type targ = Type of jtype [@@deriving gt ~options:{ show; gmap }]
  and jtype = Array of targ [@@deriving gt ~options:{ show; gmap }]]
end
(*  *)
