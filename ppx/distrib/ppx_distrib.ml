(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx

let name = "distrib"

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      pstr
        (pstr_type
           nonrecursive
           ((type_declaration_attributes __
            @@ type_declaration
                 ~name:(string "t")
                 ~params:__
                 ~cstrs:nil
                 ~kind:__
                 ~private_:__
                 ~manifest:__)
           ^:: nil)
        ^:: pstr_type
              __
              (type_declaration
                 ~name:(string "ground")
                 ~params:__
                 ~cstrs:nil
                 ~kind:__
                 ~private_:__
                 ~manifest:(some __)
              ^:: nil)
        ^:: __)
    in
    [ Extension.declare
        name
        Extension.Context.Structure_item
        pattern
        (fun
          ~loc
          ~path
          attributes1
          params1
          kind1
          private1
          manifest1
          rec_2
          params2
          kind2
          private2
          manifest2
          other_decls
        ->
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
        let items =
          List.concat
            [ Ppx_distrib_expander.process_main
                ~loc
                base_tdecl
                ( rec_2
                , type_declaration
                    ~loc
                    ~params:params2
                    ~cstrs:[]
                    ~name:(Located.mk ~loc "ground")
                    ~kind:kind2
                    ~private_:private2
                    ~manifest:(Some manifest2) )
            ; Ppx_distrib_expander.process_composable other_decls
            ]
        in
        pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items)))
    ]
  in
  Ppxlib.Driver.register_transformation ~extensions name
;;
