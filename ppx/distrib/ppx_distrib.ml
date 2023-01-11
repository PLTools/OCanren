(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2023
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx

let name = "distrib"

type ground_input =
  rec_flag
  * ((core_type * (variance * injectivity)) list * type_kind * private_flag * core_type)

type ground_input2 =
  rec_flag
  * (string * (core_type * (variance * injectivity)) list * type_kind * private_flag)

type input =
  | Explicit of
      (Ppxlib__Import.attributes
      * ((core_type * (variance * injectivity)) list
        * type_kind
        * private_flag
        * core_type option))
      * ground_input
      * structure_item list
  | Only_ground of Ppxlib__Import.attributes * ground_input2
  | Alias of (Asttypes.rec_flag * Ppxlib__Import.attributes * type_declaration)

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      let map3 ~f p =
        p |> map2 ~f:(fun a b -> a, b) |> map2 ~f:(fun (a, b) c -> f a b c)
      in
      let map3' ~f p =
        p
        |> map2' ~f:(fun loc a b -> loc, a, b)
        |> map2 ~f:(fun (loc, a, b) c -> f loc a b c)
      in
      let map4 ~f p =
        p |> map3 ~f:(fun a b c -> a, b, c) |> map2 ~f:(fun (a, b, c) d -> f a b c d)
      in
      let map4' ~f p =
        p
        |> map3' ~f:(fun loc a b c -> loc, a, b, c)
        |> map2 ~f:(fun (loc, a, b, c) d -> f loc a b c d)
      in
      let p_fully () =
        pstr_type
          nonrecursive
          ((type_declaration_attributes __
           @@ (type_declaration
                 ~name:(string "t")
                 ~params:__
                 ~cstrs:nil
                 ~kind:__
                 ~private_:__
                 ~manifest:__
              |> map4 ~f:(fun a b c d -> a, b, c, d)))
          ^:: nil)
        |> map2 ~f:(fun a b -> a, b)
      in
      let p_ground () =
        pstr_type
          __
          ((type_declaration
              ~name:(string "ground")
              ~params:__
              ~cstrs:nil
              ~kind:__
              ~private_:__
              ~manifest:(some __)
           |> map4 ~f:(fun a b c d -> a, b, c, d))
          ^:: nil)
        |> map2 ~f:(fun a b -> a, b)
      in
      let p_ground2 () =
        (* with attributes *)
        pstr_type
          __
          ((type_declaration_attributes __
           @@ (type_declaration
                 ~name:__
                 ~params:__
                 ~cstrs:nil
                 ~kind:(as__ (ptype_variant drop))
                 ~private_:__
                 ~manifest:none
              |> map4 ~f:(fun a b c d -> a, b, c, d)))
          ^:: nil)
        |> map3 ~f:(fun is_rec attrs b -> attrs, (is_rec, b))
      in
      let p_ground_alias () =
        pstr_type
          __
          ((type_declaration_attributes __
           @@ (type_declaration
                 ~name:__
                 ~params:__
                 ~cstrs:nil
                 ~kind:ptype_abstract
                 ~private_:__
                 ~manifest:(some __)
              |> map4' ~f:(fun loc name params private_ manf ->
                   let open Ppxlib.Ast_builder.Default in
                   type_declaration
                     ~loc
                     ~name:(Located.mk ~loc name)
                     ~params
                     ~cstrs:[]
                     ~private_
                     ~kind:Ptype_abstract
                     ~manifest:(Some manf))))
          ^:: nil)
        |> map3 ~f:(fun is_rec attrs tdecl -> is_rec, attrs, tdecl)
      in
      pstr (p_fully () ^:: p_ground () ^:: __)
      |> map3 ~f:(fun a b c -> Explicit (a, b, c))
      ||| (pstr (p_ground2 () ^:: nil)
          |> map1 ~f:(fun (attrs, x) -> Only_ground (attrs, x)))
      ||| (pstr (p_ground_alias () ^:: nil) |> map1 ~f:(fun x -> Alias x))
    in
    let generate ~loc base_tdecl is_rec spec_td other_decls =
      let open Ppxlib.Ast_builder.Default in
      let items =
        List.concat
          [ Ppx_distrib_expander.process_main ~loc base_tdecl (is_rec, spec_td)
          ; Ppx_distrib_expander.process_composable other_decls
          ]
      in
      pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items))
    in
    [ Extension.declare name Extension.Context.Structure_item pattern (fun ~loc ~path:_ ->
        function
        | Alias (_is_rec, ptype_attributes, tdecl) ->
          let tdecl = { tdecl with ptype_attributes } in
          let open Ppxlib.Ast_builder.Default in
          pstr_include
            ~loc
            (include_infos ~loc (pmod_structure ~loc (Reify_impl.process1 tdecl)))
        | Only_ground (attributes1, (is_rec, (name, params, kind, private1))) ->
          let open Ppxlib.Ast_builder.Default in
          let td =
            type_declaration
              ~loc
              ~name:(Located.mk ~loc name)
              ~params
              ~cstrs:[]
              ~private_:private1
              ~kind
              ~manifest:None
          in
          let td = { td with ptype_attributes = attributes1 } in
          let full_t, normal_t = Prepare_fully_abstract.run loc td in
          generate ~loc full_t is_rec normal_t []
        | Explicit
            ( (attributes1, (params1, kind1, private1, manifest1))
            , (rec_2, (params2, kind2, private2, manifest2))
            , other_decls ) ->
          let open Ppxlib.Ast_builder.Default in
          let base_tdecl =
            let td =
              type_declaration
                ~loc
                ~name:(Located.mk ~loc "t")
                ~params:params1
                ~cstrs:[]
                ~private_:private1
                ~kind:kind1
                ~manifest:manifest1
            in
            { td with ptype_attributes = attributes1 }
          in
          let spec_td =
            type_declaration
              ~loc
              ~params:params2
              ~cstrs:[]
              ~name:(Located.mk ~loc "ground")
              ~kind:kind2
              ~private_:private2
              ~manifest:(Some manifest2)
          in
          generate ~loc base_tdecl rec_2 spec_td other_decls)
    ]
  in
  Ppxlib.Driver.register_transformation ~extensions name
;;
