open Ppxlib.Ast_builder.Default

let count relation =
  let rec helper acc e =
    match e.pexp_desc with
    | Pexp_fun (_, _, _, body) -> helper (1 + acc) body
    | _ -> acc
  in
  helper 0 relation
;;
