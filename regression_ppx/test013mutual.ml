let () = print_endline "test013"

[%%ocanren_inject
type nonrec polarity =
  | Extends
  | Super
[@@deriving gt ~options:{ gmap; fmt }]]

[%%ocanren_inject type nonrec id = GT.int [@@deriving gt ~options:{ gmap; fmt }]]

(* TODO: add arrays *)

[%%distrib
type targ =
  | T of jtype
  | Wildcard of (polarity * jtype) GT.option

and jtype =
  | Array of jtype OCanren.Std.List.ground
  | Class of id * targ OCanren.Std.List.ground
  | Interface of id * targ
  | V of
      { id : id
      ; index : GT.int
      ; upb : jtype
      ; lwb : jtype GT.option
      }
  | Null
  | Intersect of jtype OCanren.Std.List.ground
[@@deriving gt ~options:{ gmap }]]

let rec pp_arg ppf : targ -> unit = function
  | Wildcard opt -> Format.fprintf ppf "Wildcard %a" ([%fmt: (polarity * 'a) GT.option] pp_typ) opt
  | T l -> Format.fprintf ppf "%a" pp_typ l

and pp_typ ppf : jtype -> unit = function
  | Array typ -> Format.fprintf ppf "(Array %a)" (GT.fmt OCanren.Std.List.ground pp_typ) typ
  | Class _ -> Format.fprintf ppf "Class "
  | Interface _ -> Format.fprintf ppf "Interface "
  | Null -> Format.fprintf ppf "Null"
  | V { id } -> Format.fprintf ppf "_.%d" id
  | Intersect _ -> Format.fprintf ppf "Intersect"
;;

open OCanren

let () =
  OCanren.(run q) (fun q -> q === !!(Wildcard !!None)) (fun rr -> rr#reify targ_prj_exn)
  |> OCanren.Stream.take
  |> Stdlib.List.iter (Format.printf "%a\n%!" pp_arg)
;;

let () =
  let open OCanren.Std in
  let exa1 : jtype_injected =
    let a : jtype_injected Std.List.injected =
      !!(V { id = !!1; index = !!2; upb = !!Null; lwb = !!None }) % nil ()
    in
    !!(Array a)
  in
  (run q) (fun q -> q === exa1) (fun rr -> rr#reify jtype_prj_exn)
  |> OCanren.Stream.take
  |> Stdlib.List.iter (Format.printf "%a\n%!" pp_typ)
;;
