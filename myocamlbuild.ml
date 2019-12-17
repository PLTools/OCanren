open Printf
open Ocamlbuild_plugin;;

open Command;;

let () = dispatch (function
  | Before_rules ->
    ()

 | After_rules ->
     ocaml_lib "src/OCanren";
     (* miniKanren related stuff*)
     (* flag ["ocamldep"; "use_mkshow"] (S [A"-ppopt";A"-I";A"-ppopt";A"plugin"; A"-ppopt";A"mkshow.cmo" ]); *)
     flag ["compile"; "use_pa_ocanren"]
       (S [ A"-ppopt";A"camlp5/pa_ocanren.cmo"]);

     flag ["compile"; "use_time_log"]
       (S [ A"-package"; A"logger.syntax"; A"-ppopt";A"-LOG"]);

     flag ["hack_pr_o"; "compile"] (S[A"-ppopt"; A"pr_o.cmo"]);
     (* flag ["compile"; "link_minikanren"] *)
     (*   (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo" *)
     (*      ; A"-ppopt";A"-L";A"-ppopt";A"plugin" *)
     (*      ] *)
     (*   ); *)

     flag ["ocaml";"compile";"native";"keep_asm"] (S[A"-S"]);

    dep ["ppx/pp_repr.native"]        ["ppx/ppx_repr.cmxa"];

    (* TODO: maybe we need to add parametrized command to prevent copy-paste *)
    flag ["ocaml";"byte";  "link";"link_ppx_gt"] (S[A"-package"; A"GT.ppx_gt"]);
    flag ["ocaml";"native";"link";"link_ppx_gt"] (S[A"-package"; A"GT.ppx_gt"]);

    flag ["ocaml";"byte";  "link";"link_ppx_distrib"] (S[A"ppxnew/ppx_distrib.cma"]);
    flag ["ocaml";"native";"link";"link_ppx_distrib"] (S[A"ppxnew/ppx_distrib.cmxa"]);

    flag ["ocaml";"byte";  "link";"link_ppx_noinjected"] (S[A"ppxnew/ppx_noinjected.cma"]);
    flag ["ocaml";"native";"link";"link_ppx_noinjected"] (S[A"ppxnew/ppx_noinjected.cmxa"]);

    flag ["ocaml";"byte";  "link";"link_ppx_repr"] (S[A"ppx/ppx_repr.cma"]);
    flag ["ocaml";"native";"link";"link_ppx_repr"] (S[A"ppx/ppx_repr.cmxa"]);

    flag ["ocaml";"byte";  "link";"link_ppx_fresh"] (S[A"ppx/ppx_fresh.cma"]);
    flag ["ocaml";"native";"link";"link_ppx_fresh"] (S[A"ppx/ppx_fresh.cmxa"]);

    flag ["ocaml"; "native"; "link";"link_ocanren_cma"] @@
      S[A"-I"; A"bundle/ocanren/"; A"OCanren.cmxa"; A"tester.cmx"];
    flag ["ocaml";   "byte"; "link";"link_ocanren_cma"] @@
      S[A"-I"; A"bundle/ocanren/"; A"OCanren.cma"; A"tester.cma"];
    flag ["ocaml"; "compile";       "link_ocanren_cma"] (S[A"-I"; A"bundle/ocanren"]);

     (* cppo-related stuff *)
     let cppo_rules ext =
       let dep   = "%(name).cppo"-.-ext
       and prod1 = "%(name: <*> and not <*.cppo>)"-.-ext
       and prod2 = "%(name: <**/*> and not <**/*.cppo>)"-.-ext in
       let cppo_rule prod env _build =
         let dep = env dep in
         let prod = env prod in
         let tags = tags_of_pathname prod ++ "cppo" in
         Cmd (S[A "cppo"; T tags; S [A "-o"; P prod]; P dep ])
       in
       rule ("cppo: *.cppo."-.-ext^" -> *."-.-ext)  ~dep ~prod:prod1 (cppo_rule prod1);
       rule ("cppo: **/*.cppo."-.-ext^" -> **/*."-.-ext)  ~dep ~prod:prod2 (cppo_rule prod2);
     in
     List.iter cppo_rules ["ml"; "mli"];

     pflag ["cppo"] "cppo_D" (fun s -> S [A "-D"; A s]) ;
     pflag ["cppo"] "cppo_U" (fun s -> S [A "-U"; A s]) ;
     pflag ["cppo"] "cppo_I" (fun s ->
       if Pathname.is_directory s then S [A "-I"; P s]
       else S [A "-I"; P (Pathname.dirname s)]
     );
     pdep ["cppo"] "cppo_I" (fun s ->
       if Pathname.is_directory s then [] else [s]) ;
     flag ["cppo"; "cppo_q"] (A "-q") ;
     flag ["cppo"; "cppo_s"] (A "-s") ;
     flag ["cppo"; "cppo_n"] (A "-n") ;
     pflag ["cppo"] "cppo_x" (fun s -> S [A "-x"; A s]);
     pflag ["cppo"] "cppo_V" (fun s -> S [A "-V"; A s]);
     flag ["cppo"; "cppo_V_OCAML"] & S [A "-V"; A ("OCAML:" ^ Sys.ocaml_version)];

     flag ["compile"; "short_paths"] & S [A "-short-paths"];
     flag ["compile"; "backtrack"] & S [A "-backtrack"];

   ()
 | _ -> ()
)
