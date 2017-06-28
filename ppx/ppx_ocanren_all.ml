(* There we register two syntax extension to be runned together (to speedup compilation) *)
open Migrate_parsetree
open Ast_405

(* fresh *)
let () = Driver.register ~name:"pa_minikanren" Versions.ocaml_405 (fun  _ _ -> Smart_logger.pa_minikanren)

(* REPR constructor for testing *)
let () = Ppx_repr.register ()

let _ = Driver.run_main ()
