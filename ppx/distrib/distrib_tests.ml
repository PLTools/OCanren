open Ppxlib.Ast_builder.Default
open Ppxlib
open Stdppx
open Ppx_distrib_expander
open Myhelpers

let%expect_test "Generation of logic types" =
  let loc = Location.none in
  let test i =
    let t2 =
      match i.pstr_desc with
      | Pstr_type (_, [ ({ ptype_manifest = Some t } as td) ]) ->
          let st = make_logic_strat_2 td in
          ltypify_exn ~loc st t
      | _ -> assert false
    in
    Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
  in
  test [%stri type t1 = (int * int) Std.List.ground];
  [%expect
    {|
    (int OCanren.logic, int OCanren.logic) OCanren.Std.Pair.logic
      OCanren.Std.List.logic |}];
  test [%stri type t2 = (int * int * int) GT.list];
  [%expect
    {|
    (int OCanren.logic * int OCanren.logic * int OCanren.logic) OCanren.logic
      OCanren.Std.List.logic |}];
  test [%stri type t2 = GT.bool * GT.int * GT.string];
  [%expect
    {|
  (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic)
    OCanren.logic
   |}];
  ()
;;

let%expect_test _ =
  let loc = Location.none in
  let test i =
    let t2 =
      match i.pstr_desc with
      | Pstr_type (_, [ { ptype_manifest = Some t } ]) ->
          let st =
            (module struct
              let sort = `Reify
              let self_typ_name = "u_logic"
              let fully_abstract_name = "u_fuly"
              let is_selfrec_name = String.equal fully_abstract_name
              let is_fully_name = is_selfrec_name
              let ground_typ_name = "u"
              let logic_typ_name = "u_logic"
              let injected_typ_name = ""

              let define_as_fuly_abstract ~loc args =
                ptyp_constr
                  ~loc
                  (Located.mk ~loc (lident_of_list [ "OCanren"; "logic" ]))
                  [ ptyp_constr
                      ~loc
                      (Located.mk ~loc (Lident fully_abstract_name))
                      (Lazy.force args)
                  ]
              ;;

              let mangle_lident = Reify_impl.create_lident_mangler sort
            end : STRAT2)
          in
          ltypify_exn ~loc st t
      | _ -> assert false
    in
    Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
  in
  Reify_impl.(config.naming_style <- New_naming);
  test [%stri type nonrec u = state u_fuly];
  [%expect {| state_logic u_fuly OCanren.logic |}];
  Reify_impl.(config.naming_style <- Old_naming);
  ()
;;

let%expect_test _ =
  let loc = Location.none in
  let tdecl =
    match [%stri type a = A [@@deriving gt ~options:{ gmap; show }]].pstr_desc with
    | Pstr_type (_, [ h ]) -> h
    | _ -> assert false
  in
  let new_attrs = List.filter_map tdecl.ptype_attributes ~f:remove_deriving_gmap in
  (* Format.printf "%a\n%!" Pprintast.type_declaration tdecl; *)
  let new_decl = { tdecl with ptype_attributes = new_attrs } in
  let new_s = pstr_type ~loc Nonrecursive [ new_decl ] in
  Format.printf "%a\n%!" Pprintast.structure_item new_s;
  [%expect {|
    type nonrec a =
      | A [@@deriving gt ~options:{ show }]|}]
;;

let%expect_test _ =
  let loc = Location.none in
  let tdecl =
    match
      [%stri
        type ('a, 'b) either =
          | A of 'a
          | B of 'b]
        .pstr_desc
    with
    | Pstr_type (_, [ h ]) -> h
    | _ -> assert false
  in
  let e = reifier_for_fully_abstract ~kind:Prj_exn tdecl in
  Format.printf "%a\n%!" Pprintast.expression e.Reifier_info.body;
  [%expect
    {|
    let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> (chain (fmapt fa fb)))|}];
  let e = reifier_for_fully_abstract ~kind:Reify tdecl in
  Format.printf "%a\n%!" Pprintast.expression e.Reifier_info.body;
  [%expect
    {|
    let open OCanren.Env.Monad in
      OCanren.Reifier.fix
        (fun _ ->
           OCanren.reify <..>
             (chain
                (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt fa fb)))))|}]
;;

let%expect_test _ =
  let loc = Location.none in
  let wrap both =
    Ast_pattern.parse
      (Ppx_distrib.classify_tdecl ())
      loc
      ~on_error:(fun () -> assert false)
      (PStr both)
      (Format.printf "%a\n%!" Ppx_distrib.pp_input)
  in
  wrap
    [%str
      type nonrec 'a t = A of 'a
      type ground = int t];
  [%expect {| Explicit |}];
  wrap
    [%str
      type nonrec 'a t = A of 'a t2
      and 'a t2 = B of 'a t];
  [%expect {| Other |}];
  wrap [%str type nonrec 'a t = A of 'a];
  [%expect {| Other |}];
  wrap [%str type nonrec 'a t = ('a * 'a) Std.List.ground];
  [%expect {| Other |}];
  wrap
    [%str
      type nonrec 'a t =
        { asdf : 'a
        ; asqqq : GT.int
        }];
  [%expect {| Other |}];
  wrap
    [%str
      type 'targ ground =
        | Array of 'targ ground
        | Var of GT.int
      [@@deriving gt ~options:{ show; fmt; gmap }]];
  [%expect {| Other |}]
;;

let%expect_test "injectify" =
  let loc = Location.none in
  let test i =
    let t2 =
      match i.pstr_desc with
      | Pstr_type (_, [ { ptype_manifest = Some t } ]) ->
          injectify
            ~loc
            (module struct
              let sort = `Injected
              let self_typ_name = "injected"
              let is_selfrec_name s = String.equal s "ground"
              let is_fully_name = String.equal "t"
              let ground_typ_name = ""
              let logic_typ_name = ""
              let injected_typ_name = "injected"
              let fully_abstract_name = "t"

              let define_as_fuly_abstract ~loc args =
                ptyp_constr
                  ~loc
                  (Located.mk ~loc (lident_of_list [ "OCanren"; "ilogic" ]))
                  [ ptyp_constr
                      ~loc
                      (Located.mk ~loc (Lident fully_abstract_name))
                      (Lazy.force args)
                  ]
              ;;

              let mangle_lident = Reify_impl.create_lident_mangler sort
            end)
            t
      | _ -> assert false
    in
    Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
  in
  test [%stri type nonrec x = GT.int];
  [%expect {| GT.int OCanren.ilogic |}];
  test [%stri type nonrec x = GT.int t];
  [%expect {|    GT.int OCanren.ilogic t OCanren.ilogic |}];
  test [%stri type nonrec ground = ground t];
  [%expect {|    injected t OCanren.ilogic |}];
  test [%stri type nonrec ground = int GT.list];
  [%expect {|    int OCanren.ilogic OCanren.Std.List.injected |}];
  test [%stri type nonrec ground = GT.int * GT.int];
  [%expect {|    (GT.int OCanren.ilogic, GT.int OCanren.ilogic) OCanren.Std.Pair.injected |}];
  test [%stri type nonrec ground = (GT.int * GT.string) Std.List.ground];
  [%expect
    {|
    (GT.int OCanren.ilogic, GT.string OCanren.ilogic) OCanren.Std.Pair.injected
      OCanren.Std.List.injected |}];
  test [%stri type nonrec ground = GT.int t];
  [%expect {| GT.int OCanren.ilogic t OCanren.ilogic |}];
  ()
;;

open Prepare_fully_abstract

let%expect_test " " =
  let loc = Location.none in
  let stru =
    [%stri
      type 'a ground =
        | A of 'a
        | B of int * 'a
        | Temp of 'a ground
      [@@deriving gt ~options:{ gmap }]]
  in
  let td =
    match stru.pstr_desc with
    | Pstr_type (_, [ t ]) -> t
    | _ -> assert false
  in
  let full_t, normal_t =
    match run loc [ td ] with
    | [ h ] -> h
    | _ -> failwith "should not happend"
  in
  Pprintast.structure
    Format.std_formatter
    [ Ast_helper.Str.type_ ~loc Nonrecursive [ full_t ]
    ; Ast_helper.Str.type_ ~loc Recursive [ normal_t ]
    ];
  [%expect
    {|
    type nonrec ('a, 'a1, 'a0) t =
      | A of 'a
      | B of 'a1 * 'a
      | Temp of 'a0 [@@deriving gt ~options:{ gmap }]
    type 'a ground = ('a, int, 'a ground) t[@@deriving gt ~options:{ gmap }] |}]
;;

let%expect_test " " =
  let loc = Location.none in
  let stru : Ppxlib.structure_item =
    [%stri
      type xyz =
        | Symb of GT.string
        | Seq of xyz Std.List.ground]
  in
  let td =
    match stru.pstr_desc with
    | Pstr_type (_, [ t ]) -> t
    | _ -> assert false
  in
  let full_t, normal_t =
    match run loc [ td ] with
    | [ h ] -> h
    | _ -> failwith "should not happend"
  in
  Pprintast.structure
    Format.std_formatter
    [ Ast_helper.Str.type_ ~loc Nonrecursive [ full_t ]
    ; Ast_helper.Str.type_ ~loc Recursive [ normal_t ]
    ];
  [%expect
    {|
    type nonrec ('a1, 'a0) t =
      | Symb of 'a1
      | Seq of 'a0
    type xyz = (GT.string, xyz Std.List.ground) t |}]
;;

let%expect_test " " =
  let loc = Location.none in
  let stru : Ppxlib.structure_item =
    [%stri
      type tt =
        | A
        | B
        | C]
  in
  let td =
    match stru.pstr_desc with
    | Pstr_type (_, [ t ]) -> t
    | _ -> assert false
  in
  let full_t, normal_t =
    match run loc [ td ] with
    | [ h ] -> h
    | _ -> failwith "should not happend"
  in
  Pprintast.structure
    Format.std_formatter
    [ Ast_helper.Str.type_ ~loc Nonrecursive [ full_t ]
    ; Ast_helper.Str.type_ ~loc Recursive [ normal_t ]
    ];
  [%expect {|
    type nonrec t =
      | A
      | B
      | C
    type tt = t |}]
;;

let%expect_test " " =
  let loc = Location.none in
  let stru : Ppxlib.structure_item =
    [%stri
      type 'a jtyp =
        | Array of 'a jtyp
        | Var of targ
        | Other of 'a]
  in
  let td =
    match stru.pstr_desc with
    | Pstr_type (_, [ t ]) -> t
    | _ -> assert false
  in
  let full_t, normal_t =
    match run loc [ td ] with
    | [ h ] -> h
    | _ -> failwith "should not happend"
  in
  Pprintast.structure
    Format.std_formatter
    [ Ast_helper.Str.type_ ~loc Nonrecursive [ full_t ]
    ; Ast_helper.Str.type_ ~loc Recursive [ normal_t ]
    ];
  [%expect
    {|
    type nonrec ('a, 'a1, 'a0) t =
      | Array of 'a1
      | Var of 'a0
      | Other of 'a
    type 'a jtyp = ('a, 'a jtyp, targ) t |}]
;;
