open Ppxlib

let count relation =
  let rec helper acc e =
    match e.pexp_desc with
    | Pexp_function (params, _, _) -> List.length params
    | _ -> 0
  in
  helper 0 relation
;;
(* asdfasdadfasdfasdf *)
