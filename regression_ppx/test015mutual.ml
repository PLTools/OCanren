let () = print_endline "test015"

module _ = struct
  [%%ocanren_inject
  type targ = Type of jtype
  and jtype = Array of targ [@@deriving gt ~options:{ show; gmap }]]
end
