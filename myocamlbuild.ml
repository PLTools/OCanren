open Printf
open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

let ends_with ~suffix s =
  let slen = String.length suffix in
  (String.length s >= slen) && (Str.last_chars s slen = suffix)

let fold f =
  let l = ref [] in
  (try while true do l @:= [f ()] done with _ -> ());
  !l

let split_comma = Str.split_delim (Str.regexp ",")

let fold_pflag scan =
  List.fold_left
    (fun acc x -> try split_comma (scan x (fun x -> x)) @ acc with _ -> acc)
    []

let ocamlfind cmd f =
  let p = Printf.sprintf in
  let cmd = List.map (p "\"%s\"") cmd in
  let cmd = p "ocamlfind query %s" (String.concat " " cmd) in
  Pack.My_unix.run_and_open cmd (fun ic -> fold (fun () -> f ic))

let link_opts prod =
  let (all_pkgs, predicates) =
    let tags = Tags.elements (tags_of_pathname prod) in
    let pkgs = fold_pflag (fun x -> Scanf.sscanf x "package(%[^)])") tags in
    let predicates = fold_pflag (fun x -> Scanf.sscanf x "predicate(%[^)])") tags in
    ("js_of_ocaml" :: pkgs, predicates)
  in

  (* Findlib usualy set pkg_* predicate for all selected packages *)
  (* It doesn't do it with 'query' command, we have to it manualy. *)
  let cmd = "-format" :: "pkg_%p" :: "-r" :: all_pkgs in
  let predicates_pkgs = ocamlfind cmd (fun ic -> input_line ic) in

  let all_predicates = String.concat "," ("javascript" :: predicates @ predicates_pkgs) in

  (* query findlib for linking option *)
  let cmd = "-o-format" :: "-r" :: "-predicates" :: all_predicates :: all_pkgs in
  ocamlfind cmd (fun ic -> A (input_line ic))


let library_index = Hashtbl.create 32
let package_index = Hashtbl.create 32
let hidden_packages: string list ref = ref []

let hide_package_contents package = hidden_packages := package :: !hidden_packages

module Ocaml_dependencies_input = struct
  let fold_dependencies = Pack.Resource.Cache.fold_dependencies
  let fold_libraries f = Hashtbl.fold f library_index
  let fold_packages f = Hashtbl.fold f package_index
end
module Ocaml_dependencies = Pack.Ocaml_dependencies.Make(Ocaml_dependencies_input)

let caml_transitive_closure = Ocaml_dependencies.caml_transitive_closure

let init_js_of_ocaml () =
  dep ["link"; "js_of_ocaml"] ["jsoo_runner/jsoo_runner.cmo"];

  let prod = "%.js" in
  let dep = "%.cmo" in

  let f env (_:builder) =
    let cmX = env "%.cmo" in
    (* let dep = env dep in *)
    let prod = env prod in
    (* let link_opts = link_opts prod in *)
    let tags = tags_of_pathname prod ++ "js_of_ocaml" in
    (* printf "link_opts = %s\n%!" link_opts; *)
    (* printf "tags      = %s\n%!" (String.concat " " tags); *)

    let libs = [] in
    let dyndeps = [] in
    let hidden_packages = [] in
    let deps =
      caml_transitive_closure ~pack_mode:true
        ~caml_obj_ext:"cmo" ~caml_lib_ext:"cma"
        ~used_libraries:libs ~hidden_packages (cmX :: dyndeps) in
    (* let deps = (List.filter (fun l -> not (List.mem l deps)) libs) @ deps in *)
    (* printf "my deps [%s]\n%!" (String.concat "; " deps); *)

    let deps = "jsoo_runner.cmo" :: deps in

    let deps = deps |> List.map (fun s ->
      if ends_with s ~suffix:".cmi"
      then
        let s2 = Bytes.copy s in
        Bytes.set s2 (String.length s-1) 'o';
        s2
      else s
    ) in
    (* printf "my deps [%s]\n%!" (String.concat "; " deps); *)

    Cmd (S ([A"jsoo_mktop"
            ;A"-verbose"
            ;A"-g"
            ;A"-safe-string"
            (* -dont-export-unit make interface visible but do not link the code *)
            ;A"-dont-export-unit";A"gc"
            ;A"-dont-export-unit";A"weak"
            ;A"-dont-export-unit";A"genlex"
            ;A"-dont-export-unit";A"lexing"
            ;A"-dont-export-unit";A"parsing"
            ;A"-dont-export-unit";A"callback"
            ;A"-dont-export-unit";A"marshal"
            (* ;A"-dont-export-unit";A"listLabels" *)
            ;A"-I";A"src"
            ;A"-jsopt";A"-I";A"-jsopt";A"src"
            ;A"-I";A"ppx"
            ;A"-jsopt";A"-I";A"-jsopt";A"ppx"
            ;A"-I";A"jsoo_runner"
            ;A"-jsopt";A"-I";A"-jsopt";A"jsoo_runner"
            ;A"-jsopt";A"--pretty --disable shortvar"

            ;A"-jsopt";A"--no-inline"
            ;A"-jsopt";A"--debug-info"
            (* ;A"-jsopt";A"--source-map" *)

            ;A"-export-unit";A"implicitPrinters"; A"src/implicitPrinters.cmo"
            ;A"-export-unit";A"miniKanrenStd";A"src/miniKanrenStd.cmo"
            ;A"-export-unit";A"MiniKanren";A"src/MiniKanren.cmo"
            ;A"-export-unit";A"jsoo_runner"
            (* ;A"jsoo_runner/jsoo_runner.cmo" *)
            ;T tags; A "-o"; P prod
            ;A"-export-package";A"js_of_ocaml.tyxml"
            (* Requried by extensions linked it toplevel.cppo.ml *)
            ;A"-export-package";A"ppx_tools"

            ;A"-jsopt";A"-I";A"-jsopt";A"../toplevel/"
            ;A"-jsopt";A"--file";A"-jsopt";A"examples.ml"
            ] @
            (List.map (fun s -> P s) deps) @
            [A"-jsopt";A"../toplevel/mystdlib.js"])
        )
  in
  rule "js_of_ocaml toplevel: *.cmo -> .js" ~dep ~prod
    f
     (* (Pack.Ocaml_compiler.byte_link "%.cmo" "%.byte") *)
  ;
  flag ["js_of_ocaml"; "pretty"] (A "--pretty");
  flag ["js_of_ocaml"; "debuginfo"] (A "--debug-info");
  flag ["js_of_ocaml"; "noinline"] (A "--no-inline");
  flag ["js_of_ocaml"; "sourcemap"] (A "--source-map");
  pflag ["js_of_ocaml"] "jsopt" (fun n -> S [A "-jsopt"; A n]);
  pflag ["js_of_ocaml"] "opt"   (fun n -> S [A "--opt"; A n]);
  pflag ["js_of_ocaml"] "set"   (fun n -> S [A "--set"; A n]);
  pflag ["js_of_ocaml"] "export_package"
    (fun findlib_name -> S [A "-export-package"; A findlib_name]);
  ()
;;
open Command;;

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
     ocaml_lib "src/MiniKanren";
     init_js_of_ocaml ();
     (* miniKanren related stuff*)
     (* flag ["ocamldep"; "use_mkshow"] (S [A"-ppopt";A"-I";A"-ppopt";A"plugin"; A"-ppopt";A"mkshow.cmo" ]); *)
     flag ["compile"; "use_pa_minikanren"]
       (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo" ]);

     (* flag ["compile"; "link_minikanren"] *)
     (*   (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo" *)
     (*      ; A"-ppopt";A"-L";A"-ppopt";A"plugin" *)
     (*      ] *)
     (*   ); *)

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
