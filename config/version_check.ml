module Library = Build_info.V1.Statically_linked_library

let version_string = function
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let my_version_compare s1 s2 =
  let normalize s =
    let tilda s =
      match String.index_from s 0 '~' with
      | exception Not_found -> s
      | len -> StringLabels.sub s ~pos:0 ~len
    in
    let dash s =
      match String.index_from s 0 '-' with
      | exception Not_found -> s
      | len -> StringLabels.sub s ~pos:0 ~len
    in
    tilda (dash s)
  in

  let to_int s = String.split_on_char '.' s |> List.map int_of_string in
  let pp_list ppf xs =
    let pp_sep ppf () = Format.fprintf ppf " " in
    Format.(pp_print_list ~pp_sep pp_print_int) ppf xs
      [@@ocaml.warning "-26"]
  in
  let rec cmp xs ys =
    (* Format.printf "xs = %a\n" pp_list xs; *)
    (* Format.printf "ys = %a\n" pp_list ys; *)
    match (xs, ys) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | h1 :: tl1, h2 :: tl2 ->
      if Int.equal h1 h2 then cmp tl1 tl2 else Int.compare h1 h2
  in
  cmp (to_int (normalize s1)) (to_int (normalize s2))

let () =
  assert (my_version_compare "0.37.0" "0.38.0" = -1);
  assert (my_version_compare "0.38.0" "0.37.0" = 1);
  assert (my_version_compare "0.38.0" "0.38.0" = 0);
  assert (my_version_compare "0.3.0" "0.38.0" = -1);
  ()

let () =
  let libs = Build_info.V1.Statically_linked_libraries.to_list () in
  let __ _ =
    let version = Build_info.V1.version () in
    Printf.printf "version: %s\n" (version_string version);
    Printf.printf "statically linked libraries:\n";
    List.iter
      (fun lib ->
        let name = Build_info.V1.Statically_linked_library.name lib in
        let version = Build_info.V1.Statically_linked_library.version lib in
        Printf.printf "- %s (%s)\n" name (version_string version))
      libs
  in
  let ppxlib =
    List.find (fun l -> String.equal (Library.name l) "ppxlib") libs
  in
  let version =
    Library.version ppxlib |> Option.get |> Build_info.V1.Version.to_string
  in
  let () =
    match my_version_compare version "0.37.0" with
    | 1 | 0 ->
      Out_channel.with_open_text "ge38.txt" (fun ch -> output_string ch "true");
      Out_channel.with_open_text "lt38.txt" (fun ch -> output_string ch "false")
    | _ ->
      Out_channel.with_open_text "ge38.txt" (fun ch -> output_string ch "false");
      Out_channel.with_open_text "lt38.txt" (fun ch -> output_string ch "true")
  in
  Out_channel.with_open_text "runned.txt" (fun ch -> output_string ch "true");
  ()
