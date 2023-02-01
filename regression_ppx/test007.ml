open OCanren

let () = print_endline "test007"

module Scheme = struct
  [%%distrib
  type ground =
    | Symb of GT.string
    | Seq of ground Std.List.ground
  [@@deriving gt ~options:{ show; gmap }]]

  let rec inhabit_list el xs =
    conde
      [ xs === Std.nil ()
      ; Fresh.two (fun h tl -> xs === Std.List.cons h tl &&& el h &&& inhabit_list el tl)
      ]
  ;;

  let rec inhabit q =
    conde
      [ Fresh.one (fun s -> q === symb s)
      ; Fresh.two (fun _ xs -> q === seq xs &&& inhabit_list inhabit xs)
      ]
  ;;

  let () =
    print_endline "Inhabitants of scheme terms:";
    run q inhabit (fun rr -> rr#reify reify)
    |> Stream.map (GT.show logic)
    |> Stream.take ~n:10
    |> List.iteri (Printf.printf "%2d: %s\n")
  ;;
end

module Scheme_rez = struct
  [%%distrib
  type nonrec ground =
    | Val of Scheme.ground
    | Closure of (GT.string * Scheme.ground) Std.List.ground * Scheme.ground
  [@@deriving gt ~options:{ gmap; show }]]

  let (_ : injected) = val_ (Scheme.symb !!"a")

  let rec inhabit q =
    conde
      [ Fresh.one (fun s -> q === val_ s &&& Scheme.inhabit s)
      ; Fresh.two (fun env f ->
          q
          === closure env f
          &&& Scheme.inhabit f
          &&& Scheme.inhabit_list
                (fun p -> Fresh.two (fun a b -> p === Std.pair a b &&& Scheme.inhabit b))
                env)
      ]
  ;;

  let () =
    print_endline "Inhabitants of scheme interpreter results:";
    run q inhabit (fun rr -> rr#reify reify)
    |> Stream.map (GT.show logic)
    |> Stream.take ~n:10
    |> List.iteri (Printf.printf "%2d: %s\n")
  ;;
end
