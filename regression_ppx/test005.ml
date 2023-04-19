open OCanren

module _ = struct
  type state = (int * int) * (int * int) GT.list [@@deriving reify]

  let () =
    OCanren.(run q)
      (fun q -> q === Std.pair (Std.pair !!1 !!2) (Std.nil ()))
      (fun rr -> rr#reify prj_exn)
    |> OCanren.Stream.hd
    |> function
    | (1, 2), [] -> ()
    | _ -> assert false
  ;;
end

module _ = struct
  [%%distrib type state = (int * int * int) GT.list]

  let () =
    OCanren.(run q)
      (fun q -> q === Std.List.cons !!(!!1, !!2, !!3) (Std.nil ()))
      (fun rr -> rr#reify prj_exn)
    |> OCanren.Stream.hd
    |> function
    | [ (1, 2, 3) ] -> ()
    | _ -> assert false
  ;;
end

let () = print_endline "test005"
