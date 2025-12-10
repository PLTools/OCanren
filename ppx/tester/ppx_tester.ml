(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2016-2025
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(**
  An extension that allows not to write errornous qh, qrh and stuff like that.
  It looks at number of lambdas in the last argument, and insert numberals as penultimate argument.

  Expands

    {[ let __ _ = [%tester run_r OCanren.reify show_intl 1 (fun q -> q === !!1)] ]}

  to

  {[
    let __ _ =
      run_r OCanren.reify show_intl 1 q qh
        ("<string repr of goal>", (fun q -> q === (!! 1)))
  ]}

*)

open Ppxlib

let string_of_expression e =
  Format.set_margin 1000;
  Format.set_max_indent 0;
  Format.asprintf "%a" Pprintast.expression e
;;

let name = "tester"

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      pstr
        (pstr_eval
           (pexp_apply
              __
              ((nolabel ** __) ^:: (nolabel ** __) ^:: (nolabel ** __) ^:: (nolabel ** __) ^:: nil
              |> map2 ~f:(fun a b -> [ a; b ])
              ||| ((nolabel ** __) ^:: (nolabel ** __) ^:: nil |> map0 ~f:[])))
           nil
        ^:: nil)
    in
    [ Extension.declare
        name
        Extension.Context.Expression
        pattern
        (fun ~loc ~path:_ runner reifier_shower n relation ->
          let open Ppxlib.Ast_builder.Default in
          let count =
            let rec helper acc e =
              match e.pexp_desc with
              | Pexp_fun (_, _, _, body) -> helper (1 + acc) body
              | _ -> acc
            in
            helper 0 relation
          in
          let middle =
            match count with
            | 0 -> failwith "Bad syntax"
            | 1 -> [ [%expr OCanren.q]; [%expr qh] ]
            | 2 -> [ [%expr OCanren.qr]; [%expr qrh] ]
            | 3 -> [ [%expr OCanren.qrs]; [%expr qrsh] ]
            | 4 -> [ [%expr OCanren.qrst]; [%expr qrsth] ]
            | _ -> failwith (Printf.sprintf "5 and more arguments are not supported")
          in
          let last =
            let s = string_of_expression relation in
            let open Ppxlib.Ast_builder.Default in
            [%expr [%e pexp_constant ~loc (Pconst_string (s, loc, None))], [%e relation]]
          in
          pexp_apply ~loc runner
          @@ List.map (fun e -> Nolabel, e)
          @@ List.concat [ reifier_shower @ [ n ]; middle; [ last ] ])
    ]
  in
  Ppxlib.Driver.register_transformation ~extensions name
;;
