(* Old style names require type name to be called 'ground'.
   But we added a renaming that forces the type name to be called 'ground'. *)
let () = print_endline "test008"

(*  *)
module _ = struct
  [%%distrib type 'a xxx = Cons of 'a * 'a xxx [@@deriving gt ~options:{ show; gmap }]]
end

module _ = struct
  [%%distrib
  type nonrec 'a key_value =
    { name : GT.int
    ; v : 'a
    }
  [@@deriving gt ~options:{ show; gmap }]]

  let () =
    let open OCanren in
    run
      q
      (fun r -> r === !!{ name = !!5; v = !!"asdf" })
      (fun r -> r#reify (key_value_prj_exn OCanren.prj_exn))
    |> OCanren.Stream.hd
    |> GT.show key_value (GT.show GT.string)
    |> print_endline
  ;;
end
