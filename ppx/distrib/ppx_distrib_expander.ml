(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2023
 *   Dmitrii Kosarev aka Kakadu, Petr Lozov
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx
open Ppxlib.Ast_builder.Default
open Ppxlib.Ast_helper
open Printf
module Format = Caml.Format
open Myhelpers

let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let nolabelize = List.map ~f:(fun e -> Nolabel, e)

let notify fmt =
  Printf.ksprintf
    (fun s ->
      let _cmd = Printf.sprintf "notify-send %S" s in
      let (_ : int) = Caml.Sys.command _cmd in
      ())
    fmt
;;

let ( @@ ) = Caml.( @@ )
let nolabel = Asttypes.Nolabel

let mangle_construct_name name =
  let low =
    String.mapi
      ~f:
        (function
         | 0 -> Char.lowercase_ascii
         | _ -> Fn.id)
      name
  in
  match low with
  | "val" | "if" | "else" | "for" | "do" | "let" | "open" | "not" -> low ^ "_"
  | _ -> low
;;

let lower_lid lid = Location.{ lid with txt = mangle_construct_name lid.Location.txt }

let has_name_attr (xs : attributes) =
  let exception Found of string in
  try
    List.iter xs ~f:(function
      | { attr_loc; attr_name = { txt = "name" }; attr_payload = PStr [ si ] } ->
        let open Ast_pattern in
        let p = pstr_eval (pexp_constant (pconst_string __ __ none)) nil in
        parse p attr_loc ~on_error:(fun _ -> ()) si (fun s -> raise (Found s))
      | _ -> ());
    None
  with
  | Found s -> Some s
;;

let decorate_with_attributes tdecl ptype_attributes = { tdecl with ptype_attributes }

module type STRAT = sig
  (*  *)

  val logic_typ_name : type_declaration -> label with_loc
  val injected_typ_name : type_declaration -> label with_loc
end

type sort =
  [ `Injected
  | `Prj_exn
  | `Reify
  ]

let stdname_of_sort = function
  | `Injected -> "injected"
  | `Prj_exn -> "ground"
  | `Reify -> "logic"
;;

(* TODO(Kakadu): Merge two strategies  *)
module type STRAT2 = sig
  val sort : sort

  (** The name of self-recursive type which should be produced *)
  val self_typ_name : string

  (** Checks if this type name should be replaced by self-recursive type *)
  val is_selfrec_name : string -> bool

  val is_fully_name : string -> bool

  (** Rewrite all other type names *)
  val mangle_lident : loc:Location.t -> core_type list lazy_t -> Longident.t -> core_type

  (* ... ground ~~> .. t OCanren.logic
      instead of
     ... ground ~~> .. logic
  *)
  val define_as_fuly_abstract : loc:Location.t -> core_type list lazy_t -> core_type
end

let create_lident_mangler sort
  : loc:Location.t -> core_type list Lazy.t -> Longident.t -> core_type
  =
  let prim_typ, add_suffix, std_suffix =
    match sort with
    | `Injected -> "ilogic", sprintf "%s_injected", "injected"
    | `Prj_exn -> "ground", Fun.id, "ground"
    | `Reify -> "logic", sprintf "%s_logic", "logic"
  in
  if Reify_impl.is_new ()
  then
    fun ~loc args -> function
      | Ldot (Lident "GT", "list") | Ldot (Ldot (Lident "Std", "List"), "ground") ->
        let l = lident_of_list [ "OCanren"; "Std"; "List"; std_suffix ] in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Ldot (Ldot (Lident "Std", "Nat"), "ground") ->
        let l = lident_of_list [ "OCanren"; "Std"; "Nat"; std_suffix ] in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | (Lident "int" as l)
      | (Lident "string" as l)
      | (Lident "bool" as l)
      | (Ldot (Lident "GT", _) as l) ->
        ptyp_constr
          ~loc
          (Located.mk ~loc (lident_of_list [ "OCanren"; prim_typ ]))
          [ ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args) ]
        (* Above is a copy-paste between two implementations *)
      | Lident s ->
        let l = Lident (add_suffix s) in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Ldot (lprefix, tname) ->
        let l = Ldot (lprefix, add_suffix tname) in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Lapply _ -> failwiths ~loc "not supported %s %d" __FILE__ __LINE__
  else
    fun ~loc args -> function
      | Ldot (Lident "GT", "list")
      | Ldot (Ldot (Lident "Std", "List"), "ground")
      | Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "List"), "ground") ->
        let l = lident_of_list [ "OCanren"; "Std"; "List"; std_suffix ] in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | (Lident "int" as l) | (Ldot (Lident "GT", _) as l) ->
        ptyp_constr
          ~loc
          (Located.mk ~loc (lident_of_list [ "OCanren"; prim_typ ]))
          [ ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args) ]
      | Lident "ground" ->
        let l = Lident std_suffix in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Ldot (prefix, "ground") ->
        let l = Ldot (prefix, std_suffix) in
        ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Lident id ->
        let msg =
          sprintf
            "In old naming only 'ground' and 'logic' typenames are allowed. What to do \
             with ident '%s' to produce '%s'?"
            id
            (add_suffix id)
        in
        ptyp_extension
          ~loc
          ( Located.mk ~loc "ocaml.error"
          , PStr
              [ pstr_eval ~loc (pexp_constant ~loc (Pconst_string (msg, loc, None))) [] ]
          )
      | Ldot _ | Lapply _ -> failwiths ~loc "not supported"
;;

let make_logic_strat_2 tdecl =
  let module I = struct
    let sort = `Reify

    let self_typ_name =
      if Reify_impl.is_new () then tdecl.ptype_name.txt ^ "_logic" else "logic"
    ;;

    let is_selfrec_name = String.equal self_typ_name

    let is_fully_name =
      if Reify_impl.is_new ()
      then String.equal (tdecl.ptype_name.txt ^ "_fuly")
      else String.equal "t"
    ;;

    let define_as_fuly_abstract ~loc args =
      ptyp_constr
        ~loc
        (Located.mk ~loc (lident_of_list [ "OCanren"; "logic" ]))
        [ (let ful_lident =
             if Reify_impl.is_new () then tdecl.ptype_name.txt ^ "_fuly" else "t"
           in
           ptyp_constr ~loc (Located.mk ~loc (Lident ful_lident)) (Lazy.force args))
        ]
    ;;

    let mangle =
      if Reify_impl.is_new ()
      then
        function
        | s -> s ^ "_logic"
      else
        function
        | "ground" -> "logic"
        | s -> s
    ;;

    let mangle_lident = create_lident_mangler `Reify
  end
  in
  (module I : STRAT2)
;;

let make_injected_strat_2 tdecl =
  let module I = struct
    let sort = `Injected

    let self_typ_name =
      if Reify_impl.is_old () then "injected" else tdecl.ptype_name.txt ^ "_injected"
    ;;

    let is_selfrec_name =
      if Reify_impl.is_old ()
      then fun s -> String.equal "ground" s || String.equal s "t"
      else String.equal (self_typ_name ^ "_fuly")
    ;;

    let is_fully_name =
      if Reify_impl.is_new ()
      then String.equal (tdecl.ptype_name.txt ^ "_fuly")
      else String.equal "t"
    ;;

    let define_as_fuly_abstract ~loc args =
      ptyp_constr
        ~loc
        (Located.mk ~loc (lident_of_list [ "OCanren"; "ilogic" ]))
        [ (let ful_lident =
             if Reify_impl.is_new () then tdecl.ptype_name.txt ^ "_fuly" else "t"
           in
           ptyp_constr ~loc (Located.mk ~loc (Lident ful_lident)) (Lazy.force args))
        ]
    ;;

    let mangle =
      if Reify_impl.is_old ()
      then
        function
        | "ground" -> "injected"
        | "t" -> "t"
        | s -> s
      else
        function
        | s -> s ^ "_injected"
    ;;

    let mangle_lident = create_lident_mangler `Injected
  end
  in
  (module I : STRAT2)
;;

include struct
  let make_typ_exn ~loc oca_logic_ident st typ =
    let (module S : STRAT2) = st in
    let rec helper t =
      match t with
      | { ptyp_desc = Ptyp_var _ } -> t
      | { ptyp_desc = Ptyp_constr ({ txt = Lident s }, xs) } when S.is_selfrec_name s ->
        ptyp_constr
          ~loc
          (Located.mk ~loc:t.ptyp_loc (Lident S.self_typ_name))
          (List.map ~f:helper xs)
      | { ptyp_desc = Ptyp_constr ({ txt; loc }, xs) } ->
        S.mangle_lident ~loc (lazy (List.map ~f:helper xs)) txt
      | t ->
        (match t.ptyp_desc with
         (* Inprinciple, special treating of Pairs is not required,
            but it is easier to study the generated code with this specialization *)
         | Ptyp_tuple [ l; r ] ->
           ptyp_constr
             ~loc
             (Located.mk
                ~loc:t.ptyp_loc
                (lident_of_list [ "OCanren"; "Std"; "Pair"; stdname_of_sort S.sort ]))
             [ helper l; helper r ]
         | Ptyp_tuple ps ->
           let loc = t.ptyp_loc in
           ptyp_constr
             ~loc
             (Located.mk ~loc (lident_of_list [ "OCanren"; stdname_of_sort S.sort ]))
             [ ptyp_tuple ~loc (List.map ~f:helper ps) ]
         | Ptyp_constr ({ txt = Ldot (Lident "GT", _) }, []) ->
           oca_logic_ident ~loc:t.ptyp_loc t
         | _ ->
           failwiths
             ~loc
             "Fallthough case with '%a'. Kind = %s. %s %d"
             Pprintast.core_type
             t
             (stdname_of_sort S.sort)
             __FILE__
             __LINE__)
    in
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = Lident id }, args) } when S.is_fully_name id ->
      S.define_as_fuly_abstract ~loc (lazy (List.map ~f:helper args))
    | { ptyp_desc = Ptyp_constr (id, args) } ->
      if true
      then helper typ
      else (
        let ttt = ptyp_constr ~loc id (List.map ~f:helper args) in
        oca_logic_ident ~loc ttt)
    | { ptyp_desc = Ptyp_tuple [ l; r ] } ->
      (* TODO(Kakadu): Eliminate copypaste *)
      ptyp_constr
        ~loc
        (Located.mk ~loc
        @@ lident_of_list [ "OCanren"; "Std"; "Pair"; stdname_of_sort S.sort ])
        (List.map ~f:helper [ l; r ])
    | { ptyp_desc = Ptyp_tuple _ } | { ptyp_desc = Ptyp_var _ } -> helper typ
    | _ ->
      failwiths
        "can't generate %s type: %a"
        (stdname_of_sort S.sort)
        Ppxlib.Pprintast.core_type
        typ
  ;;

  let ltypify_exn ~loc =
    let oca_logic_ident ~loc = Located.mk ~loc (lident_of_list [ "OCanren"; "logic" ]) in
    make_typ_exn ~loc (fun ~loc t ->
      ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ])
  ;;

  let injectify ~loc =
    let oca_logic_ident ~loc = Located.mk ~loc (lident_of_list [ "OCanren"; "ilogic" ]) in
    make_typ_exn ~loc (fun ~loc t ->
      ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ])
  ;;

  let gtypify_exn ~loc = make_typ_exn ~loc (fun ~loc:_ t -> t)

  let%expect_test _ =
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

              let mangle_lident = create_lident_mangler sort
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
    ()
  ;;
end

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

            let mangle_lident = create_lident_mangler sort
          end)
          t
      | _ -> assert false
    in
    Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
  in
  test [%stri type nonrec x = GT.int t];
  [%expect {|    GT.int OCanren.ilogic t OCanren.ilogic |}];
  test [%stri type nonrec ground = ground t];
  [%expect {|    injected t OCanren.ilogic |}];
  test [%stri type nonrec ground = int GT.list];
  [%expect {|    int OCanren.ilogic OCanren.Std.List.injected |}];
  test [%stri type nonrec ground = GT.int * GT.int];
  [%expect
    {|    (GT.int OCanren.ilogic, GT.int OCanren.ilogic) OCanren.Std.Pair.injected |}];
  test [%stri type nonrec ground = (GT.int * GT.string) Std.List.ground];
  [%expect
    {|
    (GT.int OCanren.ilogic, GT.string OCanren.ilogic) OCanren.Std.Pair.injected
      OCanren.Std.List.injected |}];
  test [%stri type nonrec ground = GT.int t];
  [%expect {| GT.int OCanren.ilogic t OCanren.ilogic |}];
  ()
;;

let manifest_of_tdecl_exn tdecl =
  match tdecl.ptype_manifest with
  | None -> failwiths ~loc:tdecl.ptype_loc "types without manifest are not allowed"
  | Some m -> m
;;

let make_strat () =
  let module M = struct
    let logic_typ_name tdecl =
      let loc = tdecl.ptype_loc in
      if Reify_impl.is_old ()
      then Located.mk ~loc "logic"
      else Located.sprintf ~loc "%s_logic" tdecl.ptype_name.txt
    ;;

    let injected_typ_name tdecl =
      let loc = tdecl.ptype_loc in
      if Reify_impl.is_old ()
      then Located.mk ~loc "injected"
      else Located.sprintf ~loc "%s_injected" tdecl.ptype_name.txt
    ;;
  end
  in
  (module M : STRAT)
;;

let process_main ~loc base_tdecl (rec_, tdecl) =
  let (module S) = make_strat () in
  let is_rec =
    match rec_ with
    | Recursive -> true
    | Nonrecursive -> false
  in
  let tdecl =
    if Reify_impl.is_new ()
    then tdecl
    else if not is_rec
    then { tdecl with ptype_name = Located.mk ~loc:tdecl.ptype_name.loc "ground" }
    else (
      let is_selfrec name = String.equal name tdecl.ptype_name.txt in
      let rec helper t =
        match t.ptyp_desc with
        | Ptyp_constr ({ txt = Lident name; loc }, ps) when is_selfrec name ->
          ptyp_constr
            ~loc:t.ptyp_loc
            (Located.mk ~loc (Lident "ground"))
            (List.map ~f:helper ps)
        | Ptyp_constr (lident, ps) ->
          ptyp_constr ~loc:t.ptyp_loc lident (List.map ~f:helper ps)
        | Ptyp_tuple ps -> ptyp_tuple ~loc:t.ptyp_loc (List.map ~f:helper ps)
        | Ptyp_any | Ptyp_var _ -> t
        | Ptyp_arrow _ | Ptyp_alias _ | Ptyp_class _ | Ptyp_object _
        | Ptyp_variant (_, _, _)
        | Ptyp_poly (_, _)
        | Ptyp_package _ | Ptyp_extension _ ->
          failwiths ~loc:t.ptyp_loc "Not supported. %s %d" __FILE__ __LINE__
      in
      { tdecl with
        ptype_name = Located.mk ~loc:tdecl.ptype_name.loc "ground"
      ; ptype_manifest = Option.map tdecl.ptype_manifest ~f:helper
      })
  in
  let ltyp =
    let ptype_attributes =
      List.filter tdecl.ptype_attributes ~f:(fun attr ->
        match attr.attr_name.txt with
        | "distrib" -> false
        | _ -> true)
    in
    let ptype_name = S.logic_typ_name tdecl in
    let ptype_manifest =
      match tdecl.ptype_manifest with
      | None -> failwiths ~loc:tdecl.ptype_loc "no manifest %s %d" __FILE__ __LINE__
      | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) ->
        Some (ltypify_exn ~loc (make_logic_strat_2 tdecl) typ)
      | t -> t
    in
    { tdecl with ptype_name; ptype_manifest; ptype_attributes }
  in
  let names = extract_names tdecl.ptype_params in
  let ityp =
    let name = S.injected_typ_name tdecl in
    let param_type_vars = List.map names ~f:Typ.var in
    let helper = injectify ~loc (make_injected_strat_2 tdecl) in
    let ptype_manifest =
      match tdecl.ptype_manifest with
      | None -> failwiths ~loc:tdecl.ptype_loc "No manifest"
      | Some { ptyp_desc = Ptyp_constr (({ txt = Lident name } as cname), args) }
        when Reify_impl.is_new () && String.equal name (tdecl.ptype_name.txt ^ "_fuly") ->
        let oca_logic_ident ~loc = Located.mk ~loc (Ldot (Lident "OCanren", "ilogic")) in
        let add_ilogic ~loc t = ptyp_constr ~loc (oca_logic_ident ~loc) [ t ] in
        add_ilogic ~loc @@ ptyp_constr ~loc cname (List.map ~f:helper args)
        |> Base.Option.some
      | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) -> Some (helper typ)
      | t ->
        (* Format.printf "HERR %s %d\n%!" __FILE__ __LINE__; *)
        t
    in
    type_declaration
      ~loc
      ~name
      ~private_:Public
      ~kind:Ptype_abstract
      ~cstrs:[]
      ~params:(List.map param_type_vars ~f:(fun x -> x, (NoVariance, NoInjectivity)))
      ~manifest:ptype_manifest
  in
  let creators =
    if Reify_impl.is_new ()
    then []
    else (
      let name cd = mangle_construct_name cd.pcd_name.txt in
      let make_stri prim_pat add_args rhs =
        [%stri let [%p prim_pat] = [%e add_args [%expr OCanren.inji [%e rhs]]]]
      in
      match base_tdecl.ptype_kind with
      | Ptype_variant cds ->
        List.map cds ~f:(fun cd ->
          let name =
            match has_name_attr cd.pcd_attributes with
            | None -> name cd
            | Some name -> name
          in
          let prim_pat = Pat.var ~loc (Located.mk ~loc name) in
          match cd.pcd_args with
          | Pcstr_tuple xs ->
            let args = List.map xs ~f:(fun _ -> Ppxlib.gen_symbol ()) in
            let add_args rhs =
              match args with
              | [] -> [%expr fun () -> [%e rhs]]
              | args ->
                List.fold_right ~init:rhs args ~f:(fun x acc ->
                  Exp.fun_ nolabel None (Pat.var (Located.mk ~loc x)) acc)
            in
            make_stri
              prim_pat
              add_args
              (Exp.construct
                 (Located.map_lident cd.pcd_name)
                 (if List.is_empty args
                 then None
                 else Some (Exp.mytuple ~loc (List.map args ~f:(Exp.lident ~loc)))))
          | Pcstr_record ls ->
            let add_args rhs =
              List.fold_right ~init:rhs ls ~f:(fun { pld_name = { txt } } acc ->
                Exp.fun_ nolabel None (Pat.var (Located.mk ~loc txt)) acc)
            in
            make_stri
              prim_pat
              add_args
              (Exp.construct
                 (Located.map_lident cd.pcd_name)
                 (Some
                    (Exp.record
                       ~loc
                       (List.map
                          ~f:(fun { pld_name } ->
                            let ident = Lident pld_name.txt in
                            let loc = pld_name.loc in
                            Located.mk ~loc ident, Exp.ident ~loc ident)
                          ls)
                       None))))
      | Ptype_record ls ->
        let add_args rhs =
          List.fold_right ~init:rhs ls ~f:(fun { pld_name = { txt } } acc ->
            Exp.fun_ nolabel None (Pat.var (Located.mk ~loc txt)) acc)
        in
        let prim_pat =
          Pat.var ~loc (Located.map_loc ~f:(fun s -> "make_" ^ s) base_tdecl.ptype_name)
        in
        [ make_stri
            prim_pat
            add_args
            (Exp.record
               ~loc
               (List.map
                  ~f:(fun { pld_name } ->
                    let ident = Lident pld_name.txt in
                    let loc = pld_name.loc in
                    Located.mk ~loc ident, Exp.ident ~loc ident)
                  ls)
               None)
        ]
      | Ptype_open | Ptype_abstract ->
        failwiths
          ~loc:base_tdecl.ptype_loc
          "%s %d Open and abstract types are not supported"
          Caml.__FILE__
          Caml.__LINE__)
  in
  let mk_arg_reifier s = sprintf "r%s" s in
  let make_reifier_gen ~kind ?(typ = None) _is_rec tdecl =
    let pat, base_reifier, name =
      match kind with
      | Reify_impl.Reify -> [%pat? reify], [%expr OCanren.reify], "reify"
      | Prj_exn -> [%pat? prj_exn], [%expr OCanren.prj_exn], "prj_exn"
    in
    let pat =
      if Reify_impl.is_old ()
      then pat
      else (
        let name =
          Printf.sprintf "%s_%s" tdecl.ptype_name.txt (Reify_impl.string_of_kind kind)
        in
        ppat_var ~loc (Located.mk ~loc name))
    in
    let manifest = manifest_of_tdecl_exn tdecl in
    let add_args rhs =
      let loc = tdecl.ptype_loc in
      Exp.funs ~loc rhs (List.map ~f:mk_arg_reifier names)
    in
    let rec helper typ : expression =
      let loc = typ.ptyp_loc in
      match typ with
      | { ptyp_desc = Ptyp_constr ({ txt = Lident ground }, _) }
        when String.equal ground tdecl.ptype_name.txt -> [%expr self]
      | { ptyp_desc = Ptyp_var s } ->
        pexp_ident ~loc (Located.mk ~loc (lident (mk_arg_reifier s)))
      | [%type: GT.int]
      | [%type: int]
      | [%type: GT.bool]
      | [%type: bool]
      | [%type: GT.string]
      | [%type: string] -> base_reifier
      | [%type: GT.int Move.ground] -> assert false
      | [%type: [%t? _arg] GT.list] ->
        failwiths
          ~loc
          "There are some issues with GT.list. Please, use fully qualified \
           OCanren.Std.List.ground for now"
      | { ptyp_desc = Ptyp_tuple [ l; r ] } ->
        let reifier =
          Exp.ident ~loc (lident_of_list [ "OCanren"; "Std"; "Pair"; name ])
        in
        [%expr [%e reifier] [%e helper l] [%e helper r]]
      | { ptyp_desc = Ptyp_tuple _ } -> failwiths ~loc "Not implemented"
      | { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, name) }, args) }
        when Reify_impl.is_new () ->
        Myhelpers.notify "%s %d" __FILE__ __LINE__;
        let tname = Format.sprintf "%s_%s" name (Reify_impl.string_of_kind kind) in
        let rhs = pexp_ident ~loc (Located.mk ~loc (Ldot (m, tname))) in
        List.fold_left ~init:rhs args ~f:(fun acc x ->
          pexp_apply ~loc acc [ nolabel, helper x ])
      | { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, _) }, args) } ->
        let rhs = pexp_ident ~loc (Located.mk ~loc (Ldot (m, name))) in
        List.fold_left ~init:rhs args ~f:(fun acc x ->
          pexp_apply ~loc acc [ nolabel, helper x ])
      | { ptyp_desc = Ptyp_constr ({ txt = Lident name }, args) }
        when Reify_impl.is_new () ->
        let tname = Format.sprintf "%s_%s" name (Reify_impl.string_of_kind kind) in
        let rhs = pexp_ident ~loc (Located.mk ~loc (Lident tname)) in
        List.fold_left ~init:rhs args ~f:(fun acc x ->
          pexp_apply ~loc acc [ nolabel, helper x ])
      | _ ->
        failwiths
          ~loc:typ.ptyp_loc
          "not supported: %a. %s %d"
          Pprintast.core_type
          typ
          __FILE__
          __LINE__
    in
    let body () =
      match manifest.ptyp_desc with
      | Ptyp_constr (_, args) ->
        let fmapt =
          pexp_apply ~loc [%expr fmapt] (List.map args ~f:(fun t -> Nolabel, helper t))
        in
        let self_pat = if is_rec then [%pat? self] else [%pat? _] in
        [%expr
          let open OCanren.Env.Monad in
          OCanren.Reifier.fix (fun [%p self_pat] ->
            [%e base_reifier]
            <..> chain
                   [%e
                     match kind with
                     | Reify ->
                       [%expr OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:[%e fmapt])]
                     | Prj_exn -> fmapt])]
      | _ ->
        failwiths ~loc:manifest.ptyp_loc "Not supported %s %d" Caml.__FILE__ Caml.__LINE__
    in
    let pat =
      match typ with
      | None -> pat
      | Some t -> ppat_constraint ~loc pat t
    in
    pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:(add_args (body ())) ]
  in
  let typ_of_decl decl =
    ptyp_constr
      ~loc
      (Located.mk ~loc (Lident decl.ptype_name.txt))
      (List.map decl.ptype_params ~f:(fun (t, _) ->
         match t.ptyp_desc with
         | Ptyp_var _v -> t
         | _ -> assert false))
  in
  let make_reifier is_rec tdecl =
    let logic_typ = typ_of_decl ltyp in
    let injected_typ = typ_of_decl ityp in
    make_reifier_gen
      ~kind:Reify
      ~typ:
        (if List.is_empty tdecl.ptype_params
        then Some [%type: ([%t injected_typ], [%t logic_typ]) OCanren.Reifier.t]
        else None)
      is_rec
      tdecl
  in
  let make_prj_exn is_rec tdecl =
    let ground_typ = typ_of_decl tdecl in
    let injected_typ = typ_of_decl ityp in
    make_reifier_gen
      ~kind:Prj_exn
      ~typ:
        (if List.is_empty tdecl.ptype_params
        then Some [%type: ([%t injected_typ], [%t ground_typ]) OCanren.Reifier.t]
        else None)
      is_rec
      tdecl
  in
  let make_fmapt tdecl =
    let names =
      extract_names (name_type_params_in_td tdecl).ptype_params
      |> List.map ~f:(fun prefix -> gen_symbol ~prefix ())
    in
    let gt_fuly_expr =
      [%expr
        GT.gmap
          [%e
            if Reify_impl.is_old ()
            then [%expr t]
            else pexp_ident ~loc (Located.mk ~loc (Lident base_tdecl.ptype_name.txt))]]
    in
    let expr = Reify_impl.make_fmapt_body ~loc gt_fuly_expr (List.length names) in
    pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:[%pat? fmapt] ~expr ]
  in
  List.concat
    [ [ pstr_type ~loc Nonrecursive [ base_tdecl ] ]
    ; [ pstr_type ~loc rec_ [ decorate_with_attributes tdecl base_tdecl.ptype_attributes ]
      ]
    ; [ pstr_type ~loc rec_ [ decorate_with_attributes ltyp base_tdecl.ptype_attributes ]
      ]
    ; [ pstr_type ~loc rec_ [ ityp ] ]
    ; [ make_fmapt base_tdecl ]
    ; [ make_prj_exn is_rec tdecl; make_reifier is_rec tdecl ]
    ; creators
    ]
;;

(* TODO(Kakadu): remember why this is really needed *)
let process_composable =
  List.map ~f:(fun tdecl ->
    let loc = tdecl.pstr_loc in
    match tdecl.pstr_desc with
    | Pstr_type (_, [ t ]) ->
      (match t.ptype_manifest with
       | Some _ ->
         pstr_type
           ~loc
           Nonrecursive
           [ { t with
               ptype_attributes =
                 [ attribute
                     ~loc
                     ~name:(Located.mk ~loc "deriving")
                     ~payload:(PStr [ [%stri reify] ])
                 ]
             }
           ]
       | None -> tdecl)
    | _ -> tdecl)
;;
