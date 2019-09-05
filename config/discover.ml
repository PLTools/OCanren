
module Cfg = Configurator.V1

let () =
  Cfg.main ~name:"ocanren" (fun cfg ->
    let camlp5_dir = String.trim @@
      Cfg.Process.run_capture_exn cfg
        "ocamlfind" ["query"; "camlp5"]
    in
    let camlp5_archives =
      List.map (fun arch -> String.concat Filename.dir_sep [camlp5_dir; arch])
        ["pa_o.cmo"; "pa_op.cmo"; "pr_o.cmo"]
    in
    let gt_archives =
      Cfg.Process.run_capture_exn cfg
        "ocamlfind" ["query"; "-pp"; "camlp5"; "-a-format"; "-predicates"; "byte"; "GT,GT.syntax.all"]
    in
    let extract = Cfg.Flags.extract_comma_space_separated_words in
    Cfg.Flags.write_lines "camlp5-flags" camlp5_archives;
    Cfg.Flags.write_lines "gt-flags" @@ extract gt_archives
  )