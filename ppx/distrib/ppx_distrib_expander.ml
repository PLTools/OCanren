(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2022
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
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
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

include struct
  let make_typ_exn ?(ccompositional = false) ~loc oca_logic_ident kind typ =
    let rec helper = function
      | [%type: int] as t -> oca_logic_ident ~loc:t.ptyp_loc t
      | t ->
        (match t.ptyp_desc with
         | Ptyp_constr ({ txt = Ldot (Lident "GT", _) }, []) ->
           oca_logic_ident ~loc:t.ptyp_loc t
         | Ptyp_constr ({ txt = Lident s }, xs)
           when String.ends_with s ~suffix:"_fuly" && Reify_impl.is_new () ->
           let tname = String.drop_suffix s (String.length "_fuly") in
           let fixed_name =
             if String.equal kind "logic" then tname ^ "_logic" else tname
           in
           ptyp_constr
             ~loc
             (Located.mk ~loc:t.ptyp_loc (Lident fixed_name))
             (List.map ~f:helper xs)
         | Ptyp_constr ({ txt = Ldot (Lident "GT", "list") }, xs) ->
           ptyp_constr
             ~loc
             (Located.mk
                ~loc:t.ptyp_loc
                (lident_of_list [ "OCanren"; "Std"; "List"; kind ]))
             (List.map ~f:helper xs)
         | Ptyp_constr ({ txt = Ldot (path, "ground") }, xs) ->
           ptyp_constr ~loc (Located.mk ~loc (Ldot (path, kind))) (List.map ~f:helper xs)
         | Ptyp_constr ({ txt = Lident "ground" }, xs) ->
           ptyp_constr ~loc (Located.mk ~loc (Lident kind)) xs
         | Ptyp_tuple [ l; r ] ->
           ptyp_constr
             ~loc
             (Located.mk
                ~loc:t.ptyp_loc
                (lident_of_list [ "OCanren"; "Std"; "Pair"; kind ]))
             [ helper l; helper r ]
         | Ptyp_constr ({ txt = Lident _ }, []) -> oca_logic_ident ~loc:t.ptyp_loc t
         | Ptyp_constr (({ txt = Lident "t" } as id), xs) ->
           oca_logic_ident ~loc:t.ptyp_loc @@ ptyp_constr ~loc id (List.map ~f:helper xs)
         | _ -> t)
    in
    match typ with
    | { ptyp_desc = Ptyp_constr (id, args) } ->
      if ccompositional
      then helper typ
      else (
        let ttt = ptyp_constr ~loc id (List.map ~f:helper args) in
        oca_logic_ident ~loc ttt)
    | { ptyp_desc = Ptyp_tuple [ l; r ] } ->
      ptyp_constr
        ~loc
        (Located.mk ~loc @@ lident_of_list [ "OCanren"; "Std"; "Pair"; kind ])
        (List.map ~f:helper [ l; r ])
    | _ -> failwiths "can't generate %s type: %a" kind Ppxlib.Pprintast.core_type typ
  ;;

  let ltypify_exn ?(ccompositional = false) ~loc typ =
    let oca_logic_ident ~loc = Located.mk ~loc (lident_of_list [ "OCanren"; "logic" ]) in
    make_typ_exn
      ~ccompositional
      ~loc
      (fun ~loc t -> ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ])
      "logic"
      typ
  ;;

  let gtypify_exn ?(ccompositional = false) ~loc typ =
    make_typ_exn ~ccompositional ~loc (fun ~loc:_ t -> t) "ground" typ
  ;;

  let%expect_test _ =
    let loc = Location.none in
    let test i =
      let t2 =
        match i.pstr_desc with
        | Pstr_type (_, [ { ptype_manifest = Some t } ]) ->
          ltypify_exn ~ccompositional:true ~loc t
        | _ -> assert false
      in
      Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
    in
    test [%stri type t1 = (int * int) Std.List.ground];
    [%expect
      {| (int OCanren.logic, int OCanren.logic) OCanren.Std.Pair.logic Std.List.logic |}];
    ()
  ;;
end

let injectify ~loc selfname typ =
  let oca_logic_ident ~loc = Located.mk ~loc (Ldot (Lident "OCanren", "ilogic")) in
  let add_ilogic ~loc t = ptyp_constr ~loc (oca_logic_ident ~loc) [ t ] in
  let rec helper t =
    (* Format.printf "HERR %a\n%!" PPP.core_type t; *)
    (* .... ground ~~~> injected ...
       .... t      ~~~> .... t ilogic
    *)
    match t with
    | [%type: GT.int]
    | [%type: int]
    | [%type: GT.string]
    | [%type: string]
    | [%type: GT.bool]
    | [%type: bool] -> [%type: [%t t] OCanren.ilogic]
    | [%type: [%t? arg] GT.list] -> [%type: [%t helper arg] OCanren.Std.List.groundi]
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (Lident "GT", _) }, []) } ->
      ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ]
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (path, ground) }, []) }
      when String.equal selfname ground ->
      ptyp_constr ~loc (Located.mk ~loc (Ldot (path, "injected"))) []
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (path, ground) }, xs) }
      when String.equal selfname ground ->
      ptyp_constr ~loc (Located.mk ~loc (Ldot (path, "injected")))
      @@ List.map ~f:helper xs
    | { ptyp_desc = Ptyp_constr ({ txt = Lident name }, xs) }
      when String.ends_with name ~suffix:"_fuly" ->
      (* New stuff *)
      add_ilogic ~loc
      @@ ptyp_constr ~loc (Located.mk ~loc (Lident name)) (List.map ~f:helper xs)
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "t" }, xs) } ->
      add_ilogic ~loc
      @@ ptyp_constr ~loc (Located.mk ~loc (Lident "t")) (List.map ~f:helper xs)
    | { ptyp_desc = Ptyp_constr ({ txt = Lident ground }, xs) }
      when String.equal selfname ground ->
      ptyp_constr ~loc (Located.mk ~loc (Lident "injected")) (List.map ~f:helper xs)
    | { ptyp_desc = Ptyp_constr ({ txt = Lident ground }, xs) } when Reify_impl.is_new ()
      ->
      let tname = Printf.sprintf "%s_injected" ground in
      ptyp_constr ~loc (Located.mk ~loc (Lident tname)) (List.map ~f:helper xs)
    | { ptyp_desc = Ptyp_var _ } -> t
    | _ ->
      Location.raise_errorf
        ~loc
        "injectify: non supported case `%a`"
        Pprintast.core_type
        t
  in
  helper typ
;;

let%expect_test "injectify" =
  let loc = Location.none in
  let test i =
    let t2 =
      match i.pstr_desc with
      | Pstr_type (_, [ { ptype_manifest = Some t } ]) -> injectify ~loc "ground" t
      | _ -> assert false
    in
    Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
  in
  test [%stri type nonrec x = GT.int t];
  [%expect {|    GT.int OCanren.ilogic t OCanren.ilogic |}];
  test [%stri type nonrec ground = ground t];
  [%expect {|    injected t OCanren.ilogic |}];
  test [%stri type nonrec ground = int GT.list];
  [%expect {|    int OCanren.ilogic OCanren.Std.List.groundi |}];
  ()
;;

type kind =
  | Reify
  | Prj_exn

let string_of_kind = function
  | Reify -> "reify"
  | Prj_exn -> "prj_exn"
;;

let manifest_of_tdecl_exn tdecl =
  match tdecl.ptype_manifest with
  | None -> failwiths ~loc:tdecl.ptype_loc "types without manifest are not allowed"
  | Some m -> m
;;

let process_main ~loc base_tdecl (rec_, tdecl) =
  let is_rec =
    match rec_ with
    | Recursive -> true
    | Nonrecursive -> false
  in
  let ltyp =
    let ptype_attributes =
      List.filter tdecl.ptype_attributes ~f:(fun attr ->
        match attr.attr_name.txt with
        | "distrib" -> false
        | _ -> true)
    in
    let ptype_name =
      if Reify_impl.is_old ()
      then Located.mk ~loc "logic"
      else Located.sprintf ~loc "%s_logic" tdecl.ptype_name.txt
    in
    let ptype_manifest =
      match tdecl.ptype_manifest with
      | None -> failwiths ~loc:tdecl.ptype_loc "no manifest %s %d" __FILE__ __LINE__
      | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) -> Some (ltypify_exn ~loc typ)
      | t -> t
    in
    { tdecl with ptype_name; ptype_manifest; ptype_attributes }
    (* else (
      let ptype_manifest =
        match tdecl.ptype_manifest with
        | None -> failwiths ~loc:tdecl.ptype_loc "no manifest %s %d" __FILE__ __LINE__
        | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) -> Some (ltypify_exn ~loc typ)
        | t -> t
      in
      { tdecl with
        ptype_name = Located.mk ~loc "logic"
      ; ptype_manifest
      ; ptype_attributes
      }) *)
  in
  let names = extract_names tdecl.ptype_params in
  let injected_typ =
    let ptype_manifest =
      match tdecl.ptype_manifest with
      | None -> failwiths ~loc:tdecl.ptype_loc "No manifest"
      | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) ->
        Some (injectify ~loc tdecl.ptype_name.txt typ)
      | t -> t
    in
    let name =
      if Reify_impl.is_old ()
      then Located.mk ~loc "injected"
      else Located.sprintf ~loc "%s_injected" tdecl.ptype_name.txt
    in
    type_declaration
      ~loc
      ~name
      ~private_:Public
      ~kind:Ptype_abstract
      ~cstrs:[]
      ~params:(List.map names ~f:(fun s -> Typ.var s, (NoVariance, NoInjectivity)))
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
      | Reify -> [%pat? reify], [%expr OCanren.reify], "reify"
      | Prj_exn -> [%pat? prj_exn], [%expr OCanren.prj_exn], "prj_exn"
    in
    let pat =
      if Reify_impl.is_old ()
      then pat
      else
        ppat_var
          ~loc
          (Located.mk
             ~loc
             (Printf.sprintf "%s_%s" tdecl.ptype_name.txt (string_of_kind kind)))
    in
    let manifest = manifest_of_tdecl_exn tdecl in
    let add_args =
      let loc = tdecl.ptype_loc in
      fun rhs ->
        List.fold_right names ~init:rhs ~f:(fun name acc ->
          [%expr fun [%p Pat.var (Located.mk ~loc (mk_arg_reifier name))] -> [%e acc]])
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
      | [%type: [%t? _arg] GT.list] ->
        failwiths
          ~loc
          "There are some issues with GT.list. Please, use fully qualified \
           OCanren.Std.List.ground for now "
      | { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, _) }, args) } ->
        let rhs = pexp_ident ~loc (Located.mk ~loc (Ldot (m, name))) in
        List.fold_left ~init:rhs args ~f:(fun acc x ->
          pexp_apply ~loc acc [ nolabel, helper x ])
      | { ptyp_desc = Ptyp_constr ({ txt = Lident name }, args) }
        when Reify_impl.is_new () ->
        let rhs = pexp_ident ~loc (Located.mk ~loc (Lident name)) in
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
    let body =
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
        failwiths
          ~loc:manifest.ptyp_loc
          "should not happen %s %d"
          Caml.__FILE__
          Caml.__LINE__
    in
    let pat =
      match typ with
      | None -> pat
      | Some t -> ppat_constraint ~loc pat t
    in
    pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:(add_args body) ]
  in
  let make_reifier is_rec tdecl =
    (* let manifest = manifest_of_tdecl_exn tdecl in *)
    let logic_typ =
      ptyp_constr
        ~loc
        (Located.mk ~loc (Lident ltyp.ptype_name.txt))
        (List.map ltyp.ptype_params ~f:(fun (t, _) ->
           match t.ptyp_desc with
           | Ptyp_var _v -> t
           | _ -> assert false))
    in
    make_reifier_gen
      ~kind:Reify
      ~typ:
        (if List.is_empty tdecl.ptype_params
        then Some [%type: (_, [%t logic_typ]) OCanren.Reifier.t]
        else None)
      is_rec
      tdecl
  in
  let make_prj_exn is_rec tdecl =
    (* let manifest = manifest_of_tdecl_exn tdecl in *)
    let ground_typ =
      ptyp_constr
        ~loc
        (Located.mk ~loc (Lident tdecl.ptype_name.txt))
        (List.map tdecl.ptype_params ~f:(fun (t, _) ->
           match t.ptyp_desc with
           | Ptyp_var _ -> t
           | _ -> assert false))
    in
    make_reifier_gen
      ~kind:Prj_exn
      ~typ:
        (if List.is_empty tdecl.ptype_params
        then Some [%type: (_, [%t ground_typ]) OCanren.Reifier.t]
        else None)
      is_rec
      tdecl
  in
  let make_fmapt tdecl =
    let names =
      extract_names (name_type_params_in_td tdecl).ptype_params
      |> List.map ~f:(fun prefix -> gen_symbol ~prefix ())
    in
    (* let add_funs rhs =
      List.fold_right
        names
        ~f:(fun name acc ->
          [%expr fun [%p ppat_var ~loc (Located.mk ~loc name)] -> [%e acc]])
        ~init:rhs
    in
    let subj = gen_symbol ~prefix:"subj" () in *)
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
    ; [ pstr_type ~loc rec_ [ injected_typ ] ]
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
