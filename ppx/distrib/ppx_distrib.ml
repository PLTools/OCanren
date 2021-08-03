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
        (pstr_type
           nonrecursive
           ((type_declaration_attributes __ @@  type_declaration
              ~name:(string "t")
              ~params:__
              ~cstrs:nil
              ~kind:__
              ~private_:__
              ~manifest:none)
           ^:: nil)
        ^:: pstr_type
              __
              (type_declaration
                 ~name:(string "ground")
                 ~params:nil
                 ~cstrs:nil
                 ~kind:__
                 ~private_:__
                 ~manifest:(some __)
              ^:: nil)
        ^:: nil)
    in
    [ Extension.declare
        name
        Extension.Context.Structure_item
        pattern
        (fun ~loc ~path attributes1 params1 kind1 private1 rec_2 kind2 private2 manifest2 ->
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
                ~manifest:None
            in
            { td with ptype_attributes = attributes1 }
          in
          let items =
            Ppx_distrib_expander.process_main
              ~loc
              base_tdecl
              ( rec_2
              , type_declaration
                  ~loc
                  ~params:[]
                  ~cstrs:[]
                  ~name:(Located.mk ~loc "ground")
                  ~kind:kind2
                  ~private_:private2
                  ~manifest:(Some manifest2) )
          in
          pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items)))
    ]
  in
  Ppxlib.Driver.register_transformation
    ~extensions
    (*
    ~impl:(fun ss ->
      let open Ppx_distrib_expander in
      let eval_si ~fail ~failtd hist si =
        match si.pstr_desc with
        | Pstr_type (flg, tydecls) ->
          List.concat
          @@ List.map tydecls ~f:(fun tydecl ->
                 if suitable_tydecl tydecl
                 then str_type_decl ~loc:si.pstr_loc (flg, tydecls)
                 else if Option.is_some @@ is_super_suitable tydecl
                 then
                   (* let () = Attribute.explicitly_drop#type_declaration tydecl in *)
                   process_super_suitable ~loc:si.pstr_loc hist (flg, tydecl)
                 else
                   [ { si with pstr_desc = Pstr_type (flg, List.map ~f:failtd tydecls) } ])
        | _ -> fail si
      in
      let m =
        object (self)
          inherit Ast_traverse.map as super

          method! structure ss =
            List.fold_left
              ss
              ~f:(fun (hist, acc) si ->
                ( si :: hist
                , acc
                  @ eval_si
                      ~failtd:super#type_declaration
                      ~fail:(fun si -> [ super#structure_item si ])
                      hist
                      si ))
              ~init:([], [])
            |> snd

          (* List.concat @@ List.map ~f:(self#do_structure_item) ss *)

          method do_structure_item si =
            eval_si
              ~failtd:super#type_declaration
              ~fail:(fun si -> [ super#structure_item si ])
              []
              si
          (*             match si.pstr_desc with
            | Pstr_type (flg,tydecls) ->
              List.concat @@ List.map tydecls
                ~f:(fun tydecl ->
                    if suitable_tydecl tydecl
                    then str_type_decl ~loc:si.pstr_loc (flg, tydecls)
                    else if is_super_suitable tydecl
                    then process_super_suitable ~loc:si.pstr_loc (flg, tydecls)
                    else [{si with
                            pstr_desc = Pstr_type (flg, List.map ~f:(super#type_declaration) tydecls)}]
                  )
            | _ -> [super#structure_item si] *)
        end
      in
      m#structure ss)
      *)
    name
;;
