(*
 * OCanren PPX
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Base
open Ppxlib

let name = "distrib"

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      pstr
        ( pstr_type nonrecursive
            ( ( type_declaration_attributes __
              @@ type_declaration ~name:(string "t") ~params:__ ~cstrs:nil
                   ~kind:__ ~private_:__ ~manifest:none )
            ^:: nil )
        ^:: pstr_type __
              ( type_declaration ~name:(string "ground") ~params:nil ~cstrs:nil
                  ~kind:__ ~private_:__ ~manifest:(some __)
              ^:: nil )
        ^:: nil ) in
    [ Extension.declare name Extension.Context.Structure_item pattern
        (fun
          ~loc
          ~path
          attributes1
          params1
          kind1
          private1
          rec_2
          kind2
          private2
          manifest2
        ->
          let open Ppxlib.Ast_builder.Default in
          let base_tdecl =
            let td =
              type_declaration ~loc ~name:(Located.mk ~loc "t") ~params:params1
                ~cstrs:[] ~private_:private1 ~kind:kind1 ~manifest:None in
            {td with ptype_attributes = attributes1} in
          let items =
            Ppx_distrib_expander.process_main ~loc base_tdecl
              ( rec_2
              , type_declaration ~loc ~params:[] ~cstrs:[]
                  ~name:(Located.mk ~loc "ground") ~kind:kind2
                  ~private_:private2 ~manifest:(Some manifest2) ) in
          pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items)) )
    ] in
  Ppxlib.Driver.register_transformation ~extensions name
