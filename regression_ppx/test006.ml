open OCanren

module Gterm = struct
  [%%distrib
  type nonrec ('s, 'xs) t =
    | Symb of 's
    | Seq of 'xs
  [@@deriving gt ~options:{ gmap }]

  type ground = (GT.string, ground Std.List.ground) t]
end

module Gresult = struct
  [%%distrib
  type nonrec ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val of 't
  [@@deriving gt ~options:{ gmap }]

  type ground = (GT.string, Gterm.ground, (GT.string, ground) Std.Pair.ground Std.List.ground) t]
end

(* Constructor arguments as a record *)
module _ = struct
  [%%distrib
  type nonrec t =
    | T of
        { x : GT.int
        ; y : GT.int
        }
  [@@deriving gt ~options:{ gmap }]

  type nonrec ground = t]
end

(* records *)
module _ = struct
  [%%distrib
  type nonrec t =
    { x : GT.int
    ; y : GT.int
    }
  [@@deriving gt ~options:{ gmap }]

  type nonrec ground = t]
end

let () = print_endline "test006"

module _ = struct
  [%%ocanren_inject
  type ground =
    | Symb of GT.string
    | Seq of ground Std.List.ground
  [@@deriving gt ~options:{ gmap }]]
end

module _ = struct
  [%%ocanren_inject type ground = GT.bool * GT.int * GT.string]

  let () =
    OCanren.(run q) (fun q -> q === inj (!!true, !!5, !!"x")) (fun rr -> rr#reify prj_exn)
    |> Stream.iter (fun (b, n, s) -> Printf.printf "%b %d %S\n" b n s)
  ;;
end

module _ = struct
  [%%ocanren_inject
  type ground = (GT.bool * GT.int * GT.string) * (GT.bool * GT.int * GT.string) [@@deriving gt]]
end
