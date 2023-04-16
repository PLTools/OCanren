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
  OCanren.(run q)
    (fun q -> q === !!TNoarg)
    (fun rr -> rr#reify (targ_prj_exn OCanren.prj_exn))
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

(* include struct
  type nonrec ('a, 'b) t =
    | [] [@name "nil"]
    | ( :: ) of 'a * 'b [@name "cons"]
  [@@deriving gt ~options:{ gmap; show }]

  type 'a ground = ('a, 'a ground) t [@@deriving gt ~options:{ gmap; show }]
  type 'a logic = ('a, 'a logic) t OCanren.logic [@@deriving gt ~options:{ gmap; show }]
  type 'a injected = ('a, 'a injected) t OCanren.ilogic

  let fmapt f__013_ f__014_ subj__015_ =
    let open OCanren.Env.Monad in
    OCanren.Env.Monad.return (GT.gmap t) <*> f__013_ <*> f__014_ <*> subj__015_
  ;;

  let prj_exn ra =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt ra self))
  ;;

  let reify ra =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      OCanren.reify
      <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra self))))
  ;;

  let nil () = OCanren.inji []
  let cons _x__009_ _x__010_ = OCanren.inji (_x__009_ :: _x__010_)
end
 *)
