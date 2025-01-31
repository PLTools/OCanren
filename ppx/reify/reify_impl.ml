(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2015-2025
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Ppxlib
open Stdppx
open Ppxlib.Ast_builder.Default
module Format = Stdlib.Format
open Myhelpers

type naming =
  | Old_naming
  | New_naming

type config = { mutable naming_style : naming }

let config = { naming_style = Old_naming }

let is_old () =
  match config.naming_style with
  | Old_naming -> true
  | New_naming -> false
;;

let is_new () = not (is_old ())

type kind =
  | Reify
  | Prj_exn

let typ_for_kind = function
  | Reify -> "logic"
  | Prj_exn -> "ground"
;;

let string_of_kind = function
  | Reify -> "reify"
  | Prj_exn -> "prj_exn"
;;

let unwrap_kind ~loc = function
  | Reify -> [%expr OCanren.reify], "reify"
  | Prj_exn -> [%expr OCanren.prj_exn], "prj_exn"
;;

module type NAME_MANGLER = sig
  val mangle_lident : loc:Location.t -> Longident.t -> ((core_type list -> core_type) -> 'a) -> 'a
end

let make_new_mangler kind tname =
  let fix_tname s =
    match kind with
    | Reify -> s ^ "_logic"
    | Prj_exn -> s
  in
  let wrap_in_logic =
    match kind with
    | Reify ->
        fun ~loc t ->
          let lid = Located.mk ~loc (lident_of_list [ "OCanren"; "logic" ]) in
          ptyp_constr ~loc lid [ t ]
    | Prj_exn -> fun ~loc:_ t -> t
  in
  let module New_mangler : NAME_MANGLER = struct
    let mangle_lident ~loc lid k =
      match lid with
      | Ldot (Ldot (Lident "Std", mname), "ground") ->
          let tail =
            match kind with
            | Reify -> "logic"
            | Prj_exn -> "ground"
          in
          let lid = Ldot (Ldot (Lident "Std", mname), tail) in
          k (ptyp_constr ~loc (Located.mk ~loc lid))
      | Ldot (Lident "GT", "string") | Ldot (Lident "GT", "bool") | Ldot (Lident "GT", "int") ->
          k (fun ps -> wrap_in_logic ~loc @@ ptyp_constr ~loc (Located.mk ~loc lid) ps)
      | Ldot (Lident "GT", "list") ->
          let lid =
            match kind with
            | Reify -> "logic"
            | Prj_exn -> "ground"
          in
          let lid = lident_of_list [ "OCanren"; "Std"; "List"; lid ] in
          k (ptyp_constr ~loc (Located.mk ~loc lid))
      | Lident l when String.equal l (tname ^ "_fuly") ->
          (* ... t_fuly -> ... t OCanren.logic *)
          k (fun ps -> wrap_in_logic ~loc @@ ptyp_constr ~loc (Located.mk ~loc (Lident tname)) ps)
      | Lident l when String.equal l tname ->
          (* ... t_fuly -> ... t_{ground/logic} *)
          let lid = Lident (fix_tname tname) in
          k (ptyp_constr ~loc (Located.mk ~loc lid))
      | Lident tname ->
          let lid = Lident (fix_tname tname) in
          k (fun ps -> ptyp_constr ~loc (Located.mk ~loc lid) ps)
      | Ldot (prefix, tname) ->
          let lid = Ldot (prefix, fix_tname tname) in
          k (fun ps -> ptyp_constr ~loc (Located.mk ~loc lid) ps)
      | _ ->
          failwiths
            "What to do with '%a'?"
            Pprintast.expression
            (pexp_ident ~loc (Located.mk ~loc lid))
    ;;
  end in
  (module New_mangler : NAME_MANGLER)
;;

include struct
  let make strat wrap_in_logic ~loc typ =
    let (module S : NAME_MANGLER) = strat in
    let rec helper = function
      | [%type: int] as t -> wrap_in_logic ~loc:t.ptyp_loc t
      | t ->
          (match t.ptyp_desc with
          | Ptyp_tuple ps ->
              wrap_in_logic ~loc:t.ptyp_loc @@ ptyp_tuple ~loc (List.map ~f:helper ps)
          | Ptyp_constr ({ txt; loc }, args) ->
              S.mangle_lident ~loc txt (fun f -> f @@ List.map ~f:helper args)
          | _ -> failwiths ~loc:t.ptyp_loc "Not implemented '%a'" Pprintast.core_type t)
    in
    helper typ
  ;;

  let ltypify_exn ~loc tname typ =
    let st = make_new_mangler Reify tname in
    let wrap_in_logic ~loc t =
      let lid = Located.mk ~loc (lident_of_list [ "OCanren"; "logic" ]) in
      ptyp_constr ~loc lid [ t ]
    in
    make st wrap_in_logic ~loc typ
  ;;

  let gtypify_exn ~loc tname typ =
    let st = make_new_mangler Prj_exn tname in
    let wrap_in_logic ~loc:_ t = t in
    make st wrap_in_logic ~loc typ
  ;;

  let%expect_test _ =
    let loc = Location.none in
    let test i =
      let t2 =
        match i.pstr_desc with
        | Pstr_type (_, [ { ptype_manifest = Some t } ]) -> ltypify_exn ~loc "asdf" t
        | _ -> assert false
      in
      Format.printf "%a\n%!" Ppxlib.Pprintast.core_type t2
    in
    test [%stri type t1 = (int * int) Std.List.ground];
    [%expect {| (int OCanren.logic * int OCanren.logic) OCanren.logic Std.List.logic |}];
    ()
  ;;
end

let make_fmapt_body ~loc gmap_expr count =
  let names = List.init ~len:count ~f:(fun _ -> gen_symbol ~prefix:"f" ()) in
  let add_funs rhs =
    List.fold_right
      names
      ~f:(fun name acc -> [%expr fun [%p ppat_var ~loc (Located.mk ~loc name)] -> [%e acc]])
      ~init:rhs
  in
  let subj = gen_symbol ~prefix:"subj" () in
  let expr =
    add_funs
      [%expr
        fun [%p ppat_var ~loc (Located.mk ~loc subj)] ->
          let open OCanren.Env.Monad in
          [%e
            List.fold_left
              ~init:[%expr OCanren.Env.Monad.return [%e gmap_expr]]
              names
              ~f:(fun acc name ->
                [%expr [%e acc] <*> [%e pexp_ident ~loc (Located.mk ~loc (Lident name))]])]
          <*> [%e pexp_ident ~loc (Located.mk ~loc (lident subj))]]
  in
  expr
;;

let make_reifier_for_tuple ~loc kind = function
  | [ _; _ ] ->
      (* Shortcut for pairs *)
      let _, reifier_name = unwrap_kind ~loc kind in
      pexp_ident
        ~loc
        (Located.mk ~loc (Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "Pair"), reifier_name)))
  | ps ->
      let gmap_expr =
        let fnames = List.mapi ps ~f:(fun i _ -> Printf.sprintf "f%d" i) in
        let subj_pat =
          ppat_tuple
            ~loc
            (List.map fnames ~f:(fun name -> ppat_var ~loc (Located.sprintf ~loc "%ss" name)))
        in
        List.fold_right
          fnames
          ~f:(fun name acc ->
            [%expr fun [%p ppat_var ~loc (Located.sprintf ~loc "%s" name)] -> [%e acc]])
          ~init:
            [%expr
              fun [%p subj_pat] ->
                [%e
                  pexp_tuple
                    ~loc
                    (List.map fnames ~f:(fun name ->
                         pexp_apply
                           ~loc
                           (pexp_ident ~loc (Located.mk ~loc @@ Lident name))
                           [ ( Asttypes.Nolabel
                             , pexp_ident ~loc (Located.mk ~loc @@ Lident (name ^ "s")) )
                           ]))]]
      in
      let rnames = List.map ~f:(fun _ -> gen_symbol ~prefix:"r" ()) ps in
      let body =
        [%expr
          let gmap_tuple = [%e gmap_expr] in
          let fmapt = [%e make_fmapt_body ~loc [%expr gmap_tuple] (List.length ps)] in
          OCanren.Reifier.fix (fun _ ->
              let open OCanren.Env.Monad in
              [%e
                let call_to_fmapt =
                  Myhelpers.Exp.apply ~loc [%expr fmapt] (List.map rnames ~f:(Exp.lident ~loc))
                in
                match kind with
                | Prj_exn -> [%expr OCanren.prj_exn <..> chain [%e call_to_fmapt]]
                | Reify ->
                    [%expr
                      OCanren.reify
                      <..> chain
                             (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:[%e call_to_fmapt]))]])]
      in
      Myhelpers.Exp.funs ~loc body rnames
;;

let create_lident_mangler sort : loc:Location.t -> core_type list Lazy.t -> Longident.t -> core_type
    =
  let prim_typ, add_suffix, std_suffix =
    match sort with
    | `Injected -> "ilogic", Printf.sprintf "%s_injected", "injected"
    | `Prj_exn -> "ground", Fun.id, "ground"
    | `Reify -> "logic", Printf.sprintf "%s_logic", "logic"
  in
  if is_new ()
  then
    fun ~loc args -> function
      | Ldot (Lident "GT", "list")
      | Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "List"), "ground")
      | Ldot (Ldot (Lident "Std", "List"), "ground") ->
          let l = lident_of_list [ "OCanren"; "Std"; "List"; std_suffix ] in
          ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Ldot (Ldot (Lident "Std", "Nat"), "ground") ->
          let l = lident_of_list [ "OCanren"; "Std"; "Nat"; std_suffix ] in
          ptyp_constr ~loc (Located.mk ~loc l) (Lazy.force args)
      | Ldot (Ldot (Lident "Std", "Option"), "ground") ->
          let l = lident_of_list [ "OCanren"; "Std"; "Option"; std_suffix ] in
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
            Printf.sprintf
              "In old naming only 'ground' and 'logic' typenames are allowed. What to do with \
               ident '%s' to produce '%s'?"
              id
              (add_suffix id)
          in
          ptyp_extension
            ~loc
            ( Located.mk ~loc "ocaml.error"
            , PStr [ pstr_eval ~loc (pexp_constant ~loc (Pconst_string (msg, loc, None))) [] ] )
      | Ldot _ | Lapply _ -> failwiths ~loc "not supported"
;;

let reifier_of_core_type ?(reifier_for_var = Fun.id) ~loc kind =
  let base_reifier, reifier_name = unwrap_kind ~loc kind in
  let on_constr ~loc lident args =
    let wrap lidents =
      Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (lident_of_list lidents))) (Lazy.force args)
    in
    match lident with
    | Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "List"), "ground")
    | Ldot (Ldot (Lident "Std", "List"), "ground")
    | Ldot (Lident "GT", "list") -> wrap [ "OCanren"; "Std"; "List"; reifier_name ]
    | Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "Option"), "ground")
    | Ldot (Ldot (Lident "Std", "Option"), "ground")
    | Ldot (Lident "GT", "option") -> wrap [ "OCanren"; "Std"; "Option"; reifier_name ]
    | Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "Nat"), "ground")
    | Ldot (Ldot (Lident "Std", "Nat"), "ground") -> wrap [ "OCanren"; "Std"; "Nat"; reifier_name ]
    | Ldot (Lident "GT", "string")
    | Lident "string"
    | [%lident GT.bool]
    | Lident "bool"
    | [%lident GT.int]
    | Lident "int" -> base_reifier
    | Lident "ground" when is_new () ->
        let reifier_name = Printf.sprintf "ground_%s" reifier_name in
        Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident reifier_name))) (Lazy.force args)
    | Lident "ground" ->
        Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident reifier_name))) (Lazy.force args)
    | Ldot (m, "ground") when is_new () ->
        let reifier_name = Printf.sprintf "ground_%s" reifier_name in
        Exp.apply
          ~loc
          (pexp_ident ~loc (Located.mk ~loc (Ldot (m, reifier_name))))
          (Lazy.force args)
    | Ldot (m, "ground") ->
        Exp.apply
          ~loc
          (pexp_ident ~loc (Located.mk ~loc (Ldot (m, reifier_name))))
          (Lazy.force args)
    | Lident tname when is_new () ->
        let reifier = Printf.sprintf "%s_%s" tname reifier_name in
        Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident reifier))) (Lazy.force args)
    | Ldot (m, tname) when is_new () ->
        let reifier = Printf.sprintf "%s_%s" tname reifier_name in
        Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (Ldot (m, reifier)))) (Lazy.force args)
    | Ldot (m, _) ->
        Exp.apply
          ~loc
          (pexp_ident ~loc (Located.mk ~loc (Ldot (m, reifier_name))))
          (Lazy.force args)
    | Lident tname when is_new () ->
        let reifier_name = Printf.sprintf "%s_%s" tname reifier_name in
        Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident reifier_name))) (Lazy.force args)
    | Lident "t" ->
        Exp.apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident reifier_name))) (Lazy.force args)
    | _ -> pexp_extension ~loc @@ Location.error_extensionf ~loc "Can't construct reifier "
  in
  let rec helper typ : expression =
    let loc = typ.ptyp_loc in
    match typ with
    | { ptyp_desc = Ptyp_var s } -> pexp_ident ~loc (Located.mk ~loc (lident (reifier_for_var s)))
    | { ptyp_desc = Ptyp_tuple ps; ptyp_loc = loc } ->
        Exp.apply ~loc (make_reifier_for_tuple ~loc kind ps) (List.map ~f:helper ps)
    | { ptyp_desc = Ptyp_constr ({ txt }, xs) } -> on_constr ~loc txt (lazy (List.map ~f:helper xs))
    | _ ->
        failwiths
          ~loc
          "Generation of compositional reifier is not yet supported for '%a'"
          Pprintast.core_type
          typ
  in
  helper
;;

let%expect_test _ =
  config.naming_style <- New_naming;
  let loc = Location.none in
  let t = [%type: 'fa tlt] in
  let e = reifier_of_core_type ~loc Prj_exn t in
  Format.printf "%a\n%!" Pprintast.expression e;
  [%expect {| tlt_prj_exn fa |}]
;;

let make_reifier_composition ~pat ?(typ = None) kind tdecl =
  let names = extract_names (name_type_params_in_td tdecl).ptype_params in
  let mk_arg_reifier = Fn.id in
  let add_args =
    let loc = tdecl.ptype_loc in
    let args rhs =
      List.fold_right names ~init:rhs ~f:(fun name acc ->
          [%expr fun [%p ppat_var ~loc (Located.mk ~loc (mk_arg_reifier name))] -> [%e acc]])
    in
    args
  in
  let helper = reifier_of_core_type kind in
  let manifest =
    match tdecl.ptype_manifest with
    | None -> failwiths "A type without manifest %s %d" Stdlib.__FILE__ Stdlib.__LINE__
    | Some m -> m
  in
  let body =
    let loc = manifest.ptyp_loc in
    match manifest.ptyp_desc with
    | Ptyp_constr (_, _args) -> helper ~loc manifest
    | Ptyp_tuple [ l; r ] ->
        let _, reifier_name = unwrap_kind ~loc kind in
        Exp.apply
          ~loc
          (Exp.ident ~loc @@ lident_of_list [ "OCanren"; "Std"; "Pair"; reifier_name ])
          [ helper ~loc l; helper ~loc r ]
    | Ptyp_tuple _ -> helper ~loc manifest
    | _ ->
        failwiths ~loc "This type is not expected as manifest %s %d" Stdlib.__FILE__ Stdlib.__LINE__
  in
  let loc = tdecl.ptype_loc in
  let pat =
    match typ with
    | None -> pat
    | Some t -> ppat_constraint ~loc pat t
  in
  pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:(add_args body) ]
;;

let make_reifier_name tdecl =
  if is_new () then Printf.sprintf "%s_reify" tdecl.ptype_name.txt else "reify"
;;

let make_prj_name tdecl =
  if is_new () then Printf.sprintf "%s_prj_exn" tdecl.ptype_name.txt else "prj_exn"
;;

let make_prj_type ~loc m tdecl =
  [%type: (_, [%t gtypify_exn tdecl.ptype_name.txt ~loc m]) OCanren.Reifier.t]
;;

let make_reifier_type ~loc m tdecl =
  [%type: (_, [%t ltypify_exn ~loc tdecl.ptype_name.txt m]) OCanren.Reifier.t]
;;

let make_reifier ~loc m tdecl =
  make_reifier_composition
    Reify
    ~typ:(if List.is_empty tdecl.ptype_params then Some (make_reifier_type ~loc m tdecl) else None)
    ~pat:(ppat_var ~loc (Located.mk ~loc (make_reifier_name tdecl)))
    tdecl
;;

let make_prj ~loc m tdecl =
  make_reifier_composition
    Prj_exn
    ~typ:(if List.is_empty tdecl.ptype_params then Some (make_prj_type ~loc m tdecl) else None)
    ~pat:(ppat_var ~loc (Located.mk ~loc (make_prj_name tdecl)))
    tdecl
;;

let process_str tdecl =
  let loc = tdecl.ptype_loc in
  match tdecl.ptype_manifest with
  | Some m ->
      (* TODO(Kakadu): find a way not to pass both manifest and type declration *)
      [ make_reifier ~loc m tdecl; make_prj ~loc m tdecl ]
  | _ -> failwiths ~loc "no manifest"
;;

let process_sig tdecl =
  let loc = tdecl.ptype_loc in
  match tdecl.ptype_manifest with
  | Some m ->
      [ (let name = Located.mk ~loc (make_prj_name tdecl) in
         psig_value ~loc
         @@ value_description ~loc ~prim:[] ~name ~type_:(make_prj_type ~loc m tdecl))
      ; (let name = Located.mk ~loc (make_reifier_name tdecl) in
         psig_value ~loc
         @@ value_description ~loc ~prim:[] ~name ~type_:(make_reifier_type ~loc m tdecl))
      ]
  | _ -> failwiths ~loc "no manifest"
;;
