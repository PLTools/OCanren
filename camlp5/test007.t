$ which pp_ocanren_all
$ ls
  $ ./pp5+ocanren+o.exe test007.ml # | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  module _ =
    struct
      [%%ocanren_inject type abc =
        A | B | C[@@deriving gt ~options:{gmap = gmap; show = show}]
      ;;]
    end
  
  module _ =
    struct
      [%%ocanren_inject type 'a maybe =
          Nothing
        | Just of 'a[@@deriving gt ~options:{gmap = gmap; show = show}]
      ;;]
    end
  
  module _ =
    struct
      [%%ocanren_inject type 'a lst =
          Nil
        | Cons of 'a * 'a lst[@@deriving gt ~options:{gmap = gmap; show = show}]
      ;;]
      let cons x xs = OCanren.inj (Cons (x, xs))
      let rec appendo x y xy =
        let open OCanren in
        conde
          [(x === inj Nil) &&& (xy === y);
           OCanren.Fresh.three
             (fun tmp h tmp2 ->
                delay
                  (fun () ->
                     conj (conj (x === cons h tmp) (xy === cons h tmp2))
                       (appendo tmp y tmp2)))]
      let () =
        OCanren.
        (run q (fun xy -> appendo (inj Nil) (cons !!1 (cons !!2 (inj Nil))) xy))
          (fun rr -> rr#reify (lst_prj_exn OCanren.prj_exn)) |>
          OCanren.Stream.iter
            (fun xs -> Format.printf "%s\n" (GT.show lst (GT.show GT.int) xs))
    end
  
  
  
  module _ =
    struct
      [%%ocanren_inject type state =
        GT.bool * GT.bool * GT.bool[@@deriving gt ~options:{gmap = gmap; show = show}]
      ;;]
      let () =
        let open OCanren in
        run q (fun q -> q === inj (!!(true), !!(true), !!(false)))
          (fun rr -> rr#reify state_prj_exn) |>
          Stream.iter (fun (a, b, c) -> Format.printf "%b %b %b\n" a b c)
    end
  
  module _ =
    struct
      [%%ocanren_inject type state =
        (GT.bool * GT.bool * GT.bool * GT.bool) *
          (GT.bool * GT.bool * GT.bool * GT.bool)[@@deriving gt ~options:{gmap = gmap; show = show}]
      ;;]
    end
  
  let () = print_endline "test007"
  $ ./test007.exe
  test007
  Cons (1, Cons (2, Nil))
  true true false
