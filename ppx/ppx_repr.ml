open Migrate_parsetree
open Ast_405

let migrate = Versions.migrate (module OCaml_405) (module OCaml_current)

let string_of_expression e =
  Format.set_margin 1000;
  let ans = Format.asprintf "%a" Pprintast.expression (migrate.Versions.copy_expression e) in
  Format.set_margin 80;
  ans

let mapper _config _cookies =
  let open Ast_mapper in
  let open Parsetree in
  let open Asttypes in
  let open Longident in

  { default_mapper
    with
    expr = fun mapper ->
           function
           | { pexp_desc=Pexp_construct ({txt=Lident "REPR";_}, Some e); _} as expr ->
              let text = string_of_expression e in
              { expr with pexp_desc =
                            Pexp_tuple [Ast_helper.Exp.constant (Pconst_string (text,None)); e] }
           | e -> default_mapper.expr mapper e
  }

let register () = Driver.register ~name:"repr" Versions.ocaml_405 mapper
