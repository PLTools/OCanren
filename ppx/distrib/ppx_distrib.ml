open Base
open Ppxlib
open Printf

let name = "distrib"

let () =
  Ppxlib.Driver.register_transformation
    ~impl:(fun ss ->
        let open Ppx_distrib_expander in
        let m = object(self)
          inherit Ast_traverse.map as super
          method! structure ss = (* TODO: Maybe we don't need this *)
            List.concat @@ List.map ~f:(self#do_structure_item) ss

          method do_structure_item si =
            match si.pstr_desc with
            | Pstr_type (flg,tydecls) ->
              List.concat @@ List.map tydecls
                ~f:(fun tydecl ->
                    if suitable_tydecl tydecl
                    then str_type_decl ~loc:si.pstr_loc (flg, tydecls)
                    else [{si with
                            pstr_desc = Pstr_type (flg, List.map ~f:(super#type_declaration) tydecls)}]
                  )
            | _ -> [super#structure_item si]
        end in
      m#structure ss
    )
    name
