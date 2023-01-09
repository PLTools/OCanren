module _ = struct
  ocanren type abc = A | B | C
end

module _ = struct
  ocanren type 'a maybe = Nothing | Just of 'a
end

module _ = struct
  ocanren type 'a lst = Nil | Cons of 'a * 'a lst

  
  let rec appendo x y xy =
    let open OCanren in
    conde
      [
        x === inj Nil &&& (xy === y);
        fresh (tmp h tmp2)
          (x === cons h tmp)
          (xy === cons h tmp2)
          (appendo tmp y tmp2);
      ]

  let () =
    OCanren.(
      run q (fun xy -> appendo (inj Nil) (cons !!1 (cons !!2 (inj Nil))) xy))
      (fun rr -> rr#reify (prj_exn OCanren.prj_exn))
    |> OCanren.Stream.iter (fun xs ->
           Format.printf "%s\n" (GT.show lst (GT.show GT.int) xs))
end

let () = print_endline "test007"
