open Migrate_parsetree
open Ast_405

(* Just syntax extension for fresh *)
let () = Smart_logger.register ()

let _ = Driver.run_main ()
