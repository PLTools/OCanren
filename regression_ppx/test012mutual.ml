let () = print_endline "test012"

[%%distrib
type 'a targ =
  | T of 'a jtyp * 'a
  | TNoarg

and 'a jtyp =
  | Array of 'a jtyp
  | V of 'a targ
  | Other of 'a
[@@deriving gt ~options:{ gmap }]]

let rec pp_arg fa ppf : 'a targ -> unit = function
  | TNoarg -> Format.fprintf ppf "noarg"
  | T (l, r) -> Format.fprintf ppf "(%a,%a)" (pp_typ fa) l fa r

and pp_typ fa ppf : 'a jtyp -> unit = function
  | Array typ -> Format.fprintf ppf "(Array %a)" (pp_typ fa) typ
  | V arg -> Format.fprintf ppf "%a" (pp_arg fa) arg
  | Other a -> Format.fprintf ppf "(Other %a)" fa a
;;

open OCanren

let () =
  OCanren.(run q) (fun q -> q === !!TNoarg) (fun rr -> rr#reify (targ_prj_exn OCanren.prj_exn))
  |> OCanren.Stream.take
  |> Stdlib.List.iter (Format.printf "%a\n%!" (pp_arg Format.pp_print_int))
;;

let () =
  OCanren.(run q)
    (fun q -> q === !!(Array !!(V !!(T (!!(Other !!1), !!2)))))
    (fun rr -> rr#reify (jtyp_prj_exn OCanren.prj_exn))
  |> OCanren.Stream.take
  |> Stdlib.List.iter (Format.printf "%a\n%!" (pp_typ Format.pp_print_int))
;;

include struct
  [%%ocanren_inject
  type 'a t =
    | [] [@name "nil"]
    | ( :: ) of 'a * 'a t [@name "cons"]
  [@@deriving gt ~options:{ gmap; show }]]

  let nil () = OCanren.inj []
  let cons _x__009_ _x__010_ = OCanren.inj (_x__009_ :: _x__010_)
end
