(* Old style names require type name to be called 'ground'.
   But we added a renaming that forces the type name to be called 'ground'. *)
let () = print_endline "test008"

(*  *)
module _ = struct
  [%%distrib type 'a xxx = Cons of 'a * 'a xxx [@@deriving gt ~options:{ show; gmap }]]
end
