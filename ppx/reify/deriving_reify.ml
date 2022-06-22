(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2015-2022
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

module Pprintast_ = Pprintast
open Ppxlib
open Stdppx
open Ppxlib.Ast_builder.Default
module Format = Caml.Format
open Myhelpers

let failwiths ?(loc = Location.none) fmt = Location.raise_errorf ~loc fmt

include struct
  let make_typ_exn ?(ccompositional = false) ~loc oca_logic_ident kind typ =
    let rec helper = function
      | [%type: int] as t -> oca_logic_ident ~loc:t.ptyp_loc t
      | t ->
        (match t.ptyp_desc with
        | Ptyp_constr ({ txt = Ldot (Lident "GT", s) }, []) ->
          oca_logic_ident ~loc:t.ptyp_loc t
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
        | Ptyp_constr ({ txt = Lident s }, []) -> oca_logic_ident ~loc:t.ptyp_loc t
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
    | _ ->
      Location.raise_errorf
        ~loc
        "can't generate %s type: %a"
        kind
        Ppxlib.Pprintast.core_type
        typ
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
    make_typ_exn ~ccompositional ~loc (fun ~loc t -> t) "ground" typ
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

type kind =
  | Reify
  | Prj_exn

let unwrap_kind ~loc = function
  | Reify -> [%expr OCanren.reify], "reify"
  | Prj_exn -> [%expr OCanren.prj_exn], "prj_exn"
;;

let reifier_of_core_type ~loc kind =
  let base_reifier, reifier_name = unwrap_kind ~loc kind in
  let rec helper typ =
    let loc = typ.ptyp_loc in
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (Lident "GT", "list") }, xs) } ->
      Exp.apply
        ~loc
        (pexp_ident
           ~loc
           (Located.mk ~loc (lident_of_list [ "Std"; "List"; reifier_name ])))
        (List.map xs ~f:helper)
    | [%type: GT.string]
    | [%type: string]
    | [%type: GT.bool]
    | [%type: bool]
    | [%type: GT.int]
    | [%type: int] -> base_reifier
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "ground" }, xs) } ->
      Exp.apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (lident reifier_name)))
        (List.map ~f:helper xs)
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, "ground") }, xs) } ->
      Exp.apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (Ldot (m, reifier_name))))
        (List.map xs ~f:helper)
    | { ptyp_desc = Ptyp_var s } -> pexp_ident ~loc (Located.mk ~loc (lident s))
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (Lident m, _) }, args) } ->
      pexp_apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (Ldot (lident m, reifier_name))))
        (List.map args ~f:(fun t -> Nolabel, helper t))
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "t" }, args) } ->
      pexp_apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (Lident reifier_name)))
        (List.map args ~f:(fun t -> Nolabel, helper t))
    | { ptyp_desc = Ptyp_tuple [ l; r ] } ->
      Exp.apply
        ~loc
        (pexp_ident
           ~loc
           (Located.mk ~loc (Ldot (Ldot (Lident "Std", "Pair"), reifier_name))))
        [ helper l; helper r ]
    | _ -> failwiths ~loc "Generation of compositional reifier is not supported yet"
  in
  helper
;;

let make_reifier_composition ~pat ?(typ = None) kind tdecl =
  let names = extract_names @@ (name_type_params_in_td tdecl).ptype_params in
  let mk_arg_reifier = Fn.id in
  let add_args =
    let loc = tdecl.ptype_loc in
    let args rhs =
      List.fold_right names ~init:rhs ~f:(fun name acc ->
          [%expr
            fun [%p ppat_var ~loc (Located.mk ~loc (mk_arg_reifier name))] -> [%e acc]])
    in
    args
  in
  let helper = reifier_of_core_type kind in
  let manifest =
    match tdecl.ptype_manifest with
    | None -> failwiths "A type without manifest %s %d" Caml.__FILE__ Caml.__LINE__
    | Some m -> m
  in
  let body =
    let loc = manifest.ptyp_loc in
    match manifest.ptyp_desc with
    | Ptyp_constr ({ txt }, args) -> helper ~loc manifest
    | Ptyp_tuple [ l; r ] ->
      let base_reifier, reifier_name = unwrap_kind ~loc kind in
      Exp.apply
        ~loc
        (Exp.ident ~loc @@ lident_of_list [ "OCanren"; "Std"; "Pair"; reifier_name ])
        [ helper ~loc l; helper ~loc r ]
    | _ ->
      failwiths
        ~loc
        "This type is not expected as manifest %s %d"
        Caml.__FILE__
        Caml.__LINE__
  in
  let loc = tdecl.ptype_loc in
  let pat =
    match typ with
    | None -> pat
    | Some t -> ppat_constraint ~loc pat t
  in
  pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:(add_args body) ]
;;

let process1 tdecl =
  let loc = tdecl.ptype_loc in
  match tdecl.ptype_manifest with
  | Some m ->
    (* TODO: find a way not to pass both manifest and type declration *)
    [ make_reifier_composition
        Reify
        ~typ:
          (if List.is_empty tdecl.ptype_params
          then Some [%type: (_, [%t ltypify_exn ~ccompositional:true ~loc m]) Reifier.t]
          else None)
        ~pat:
          (ppat_var
             ~loc
             (Located.mk ~loc @@ Format.sprintf "reify_%s" tdecl.ptype_name.txt))
        tdecl
    ; make_reifier_composition
        Prj_exn
        ~typ:
          (if List.is_empty tdecl.ptype_params
          then Some [%type: (_, [%t gtypify_exn ~ccompositional:true ~loc m]) Reifier.t]
          else None)
        ~pat:
          (ppat_var
             ~loc
             (Located.mk ~loc @@ Format.sprintf "prj_exn_%s" tdecl.ptype_name.txt))
        tdecl
    ]
  | None -> failwiths ~loc "no manifest"
;;

let str_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.empty (fun ~loc ~path (_, info) ->
      List.concat_map info ~f:process1)
;;

let () =
  Deriving.add "reify" ~str_type_decl ~extension:(fun ~loc ~path:_ ->
      reifier_of_core_type ~loc Reify)
  |> Deriving.ignore
;;

let () =
  Deriving.add "prj_exn" ~extension:(fun ~loc ~path:_ ->
      reifier_of_core_type ~loc Prj_exn)
  |> Deriving.ignore
;;
