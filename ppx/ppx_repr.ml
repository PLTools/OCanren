open Asttypes
open Ast_mapper
open Parsetree
open Longident
open Ast_helper
open Pprintast


let mapper =
  { default_mapper with
    expr = fun mapper ->
           function
           | { pexp_desc=Pexp_construct ({txt=Lident "REPR";_}, Some e); _} as expr ->
              let text = Pprintast.string_of_expression e in
              { expr with pexp_desc =
                            Pexp_tuple [Exp.constant (Pconst_string (text,None)); e] }
           | e -> default_mapper.expr mapper e
  }
