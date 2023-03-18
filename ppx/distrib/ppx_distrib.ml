(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2023
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx

type ground_input =
  rec_flag
  * ((core_type * (variance * injectivity)) list * type_kind * private_flag * core_type)

type input =
  | Explicit of
      (Ppxlib__Import.attributes
      * ((core_type * (variance * injectivity)) list
        * type_kind
        * private_flag
        * core_type option))
      * ground_input
      * structure_item list
  | Only_ground of rec_flag * Ppxlib__Import.attributes * type_declaration
  | Alias of (Asttypes.rec_flag * Ppxlib__Import.attributes * type_declaration)
  | Alias_of_record of
      (Asttypes.rec_flag
      * Ppxlib__Import.attributes
      * type_declaration
      * label_declaration list)

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
      let map5 ~f p =
        p |> map3 ~f:(fun a b c -> a, b, c) |> map3 ~f:(fun (a, b, c) d e -> f a b c d e)
      in
      let map4' ~f p =
        p
        |> map3' ~f:(fun loc a b c -> loc, a, b, c)
        |> map2 ~f:(fun (loc, a, b, c) d -> f loc a b c d)
      in
      let p_fully () =
        as__
        @@ pstr_type
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
        |> map3 ~f:(fun _ a b -> a, b)
      in
      let p_ground () =
        as__
        @@ pstr_type
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
        |> map3 ~f:(fun _ a b -> a, b)
      in
      let p_ground2 () =
        as__
        @@ (* with attributes *)
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
              |> map4' ~f:(fun loc name params kind private_ ->
                   let open Ppxlib.Ast_builder.Default in
                   type_declaration
                     ~loc
                     ~name:(Located.mk ~loc name)
                     ~params
                     ~cstrs:[]
                     ~private_
                     ~kind
                     ~manifest:None)))
          ^:: nil)
        |> map4 ~f:(fun __ is_rec attrs b -> attrs, (is_rec, b))
      in
      let p_ground_alias () =
        as__ (* Extra hack to delay application in case of no any holes *)
        @@ pstr_type
             __
             ((type_declaration_attributes __
              @@ as__
              @@ type_declaration
                   ~name:drop
                   ~params:drop
                   ~cstrs:nil
                   ~kind:ptype_abstract
                   ~private_:drop
                   ~manifest:(some __))
             ^:: nil)
        |> map5 ~f:(fun _ is_rec attrs tdecl _manifest ->
             (* let _ = Sys.command (Printf.sprintf "notify-send %s %d" __FILE__ __LINE__) in *)
             (* Format.printf "\t\t\t\t@[%a@]" Pprintast.structure_item sitem; *)
             is_rec, attrs, tdecl)
      in
      let p_ground_record () =
        pstr_type
          __
          ((type_declaration_attributes __
           @@ (type_declaration
                 ~name:__
                 ~params:__
                 ~cstrs:nil
                 ~kind:(ptype_record __)
                 ~private_:__
                 ~manifest:none
              |> map4' ~f:(fun loc name params fields private_ ->
                   let open Ppxlib.Ast_builder.Default in
                   ( fields
                   , type_declaration
                       ~loc
                       ~name:(Located.mk ~loc name)
                       ~params
                       ~cstrs:[]
                       ~private_
                       ~kind:(Ptype_record fields)
                       ~manifest:None ))))
          ^:: nil)
        |> map3 ~f:(fun is_rec attrs (fields, tdecl) -> is_rec, attrs, tdecl, fields)
      in
      pstr
        (p_fully () ^:: p_ground () ^:: __
        |> map3 ~f:(fun a b c -> Explicit (a, b, c))
        ||| (p_ground2 () ^:: nil
            |> map1 ~f:(fun (attrs, (flag, x)) -> Only_ground (flag, attrs, x)))
        ||| (p_ground_alias () ^:: nil |> map1 ~f:(fun x -> Alias x))
        ||| (p_ground_record () ^:: nil |> map1 ~f:(fun x -> Alias_of_record x))
            (*************************************************))
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
    let make_extension name =
      Extension.declare name Extension.Context.Structure_item pattern (fun ~loc ~path:_ ->
        function
        | Alias_of_record (is_rec, ptype_attributes, tdecl, _) ->
          let full_t, normal_t =
            Prepare_fully_abstract.run loc { tdecl with ptype_attributes }
          in
          generate ~loc full_t is_rec normal_t []
        | Alias (is_rec, ptype_attributes, tdecl) ->
          let tdecl = { tdecl with ptype_attributes } in
          let open Ppxlib.Ast_builder.Default in
          let ltyp_decl =
            let open Ppx_distrib_expander in
            let abbrev =
              ltypify_exn
                ~loc
                (make_logic_strat_2 tdecl)
                (Stdlib.Option.get tdecl.ptype_manifest)
            in
            let (module S : STRAT) = make_strat () in
            { tdecl with
              ptype_name = S.logic_typ_name tdecl
            ; ptype_manifest = Some abbrev
            }
          in
          let stru =
            pstr_type ~loc is_rec [ tdecl ]
            :: pstr_type ~loc is_rec [ ltyp_decl ]
            :: Reify_impl.process1 tdecl
          in
          pstr_include ~loc (include_infos ~loc (pmod_structure ~loc stru))
        | Only_ground (is_rec, attributes1, tdecl) ->
          let tdecl = { tdecl with ptype_attributes = attributes1 } in
          let full_t, normal_t = Prepare_fully_abstract.run loc tdecl in
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
    in
    [ make_extension "distrib"; make_extension "ocanren_inject"; ]
  in
  Ppxlib.Driver.register_transformation ~extensions "distrib"
;;

let () =
  Ppxlib.Driver.add_arg
    "-new-typenames"
    (Stdlib.Arg.Unit (fun () -> Reify_impl.config.naming_style <- New_naming))
    ~doc:" Doc here"
;;
