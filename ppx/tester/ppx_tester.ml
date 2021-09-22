(*
 * OCanren PPX
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(*
  An extension that allows not to write errornous qh, qrh and stuff like that.

  Expands

    let __ _ = [%tester runR OCanren.reify show_int show_intl (fun q -> q === !!1)]

  to

    let __ _ = runR OCanren.reify show_int show_intl q qh (fun q -> q === (!! 1))

*)
open Base
open Ppxlib

let name = "tester"

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      pstr (pstr_eval (pexp_apply __ __) nil ^:: nil) in
    [ Extension.declare name Extension.Context.Expression pattern
        (fun ~loc ~path f args ->
          let open Ppxlib.Ast_builder.Default in
          let prefix, last =
            match args with
            | [] -> failwith "should not happen"
            | xs -> (List.drop_last_exn xs, List.last_exn xs) in
          let count =
            let rec helper acc e =
              match e.pexp_desc with
              | Pexp_fun (_, _, _, body) -> helper (1 + acc) body
              | _ -> acc in
            helper 0 (snd last) in
          let middle =
            List.map ~f:(fun e -> (Nolabel, e))
            @@
            match count with
            | 0 -> failwith "Bad syntax"
            | 1 -> [[%expr q]; [%expr qh]]
            | 2 -> [[%expr qr]; [%expr qrh]]
            | 3 -> [[%expr qrs]; [%expr qrsh]]
            | 4 -> [[%expr qrst]; [%expr qrsth]]
            | _ ->
                failwith
                  (Printf.sprintf "5 and more arguments are not supported")
          in
          pexp_apply ~loc f (List.concat [prefix; middle; [last]]) ) ] in
  Ppxlib.Driver.register_transformation ~extensions name
