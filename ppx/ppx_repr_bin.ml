open Migrate_parsetree

let () =
  Ppx_repr.register ();
  Driver.run_main ()
