(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren. PPX syntax extensions.
 * Copyright (C) 2015-2023
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
module Format = Caml.Format
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

let string_of_kind = function
  | Reify -> "reify"
  | Prj_exn -> "prj_exn"
;;

let unwrap_kind ~loc = function
  | Reify -> [%expr OCanren.reify], "reify"
  | Prj_exn -> [%expr OCanren.prj_exn], "prj_exn"
;;

module type NAME_MANGLER = sig
  val mangle_lident
    :  loc:Location.t
    -> Longident.t
    -> ((core_type list -> core_type) -> 'a)
    -> 'a
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
  match kind with
  | Prj_exn | Reify ->
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
        | Ldot (Lident "GT", "string")
        | Ldot (Lident "GT", "bool")
        | Ldot (Lident "GT", "int") ->
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
          k (fun ps ->
            wrap_in_logic ~loc @@ ptyp_constr ~loc (Located.mk ~loc (Lident tname)) ps)
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
            "What to do with '%a'"
            Pprintast.expression
            (pexp_ident ~loc (Located.mk ~loc lid))
      ;;
    end
    in
    (module New_mangler : NAME_MANGLER)
;;

include struct
  (*
  let make_typ_exn ?(ccompositional = false) ~loc oca_logic_ident (kind : kind) typ =
    let rec helper = function
      | [%type: int] as t -> oca_logic_ident ~loc:t.ptyp_loc t
      | t ->
        (match t.ptyp_desc with
         | Ptyp_constr ({ txt = Ldot (Lident "GT", _) }, []) ->
           oca_logic_ident ~loc:t.ptyp_loc t
         | Ptyp_constr ({ txt = Ldot (Lident "GT", "list") }, xs) ->
           ptyp_constr
             ~loc
             (Located.mk
                ~loc:t.ptyp_loc
                (lident_of_list [ "OCanren"; "Std"; "List"; typ_for_kind ]))
             (List.map ~f:helper xs)
         | Ptyp_constr ({ txt = Ldot (path, "ground") }, xs) ->
           ptyp_constr
             ~loc
             (Located.mk ~loc (Ldot (path, typ_for_kind)))
             (List.map ~f:helper xs)
         (*          | Ptyp_constr ({ txt = Lident "ground" }, xs) ->
           ptyp_constr ~loc (Located.mk ~loc (Lident typ_for_kind)) xs *)
         | Ptyp_constr (({ txt = Lident "t" } as id), xs) when is_old () ->
           oca_logic_ident ~loc:t.ptyp_loc @@ ptyp_constr ~loc id (List.map ~f:helper xs)
         | Ptyp_constr ({ txt = Lident cname }, ps) ->
           if is_old ()
           then
             oca_logic_ident ~loc:t.ptyp_loc
             @@ ptyp_constr
                  ~loc
                  (Located.mk ~loc:t.ptyp_loc (Lident cname))
                  (List.map ~f:helper ps)
           else
             ptyp_constr
               ~loc
               (Located.mk ~loc:t.ptyp_loc (Lident (fix_tname cname)))
               (List.map ~f:helper ps)
         | Ptyp_tuple [ l; r ] ->
           ptyp_constr
             ~loc
             (Located.mk
                ~loc:t.ptyp_loc
                (lident_of_list [ "OCanren"; "Std"; "Pair"; typ_for_kind ]))
             [ helper l; helper r ]
         | Ptyp_tuple xs ->
           oca_logic_ident ~loc:t.ptyp_loc @@ ptyp_tuple ~loc (List.map ~f:helper xs)
         | _ -> t)
    in
    match typ with
    (*     | { ptyp_desc = Ptyp_constr (({ txt = Lident id } as lident), args) } ->
      if ccompositional
      then helper typ
      else if is_old ()
      then (
        let ttt = ptyp_constr ~loc lident (List.map ~f:helper args) in
        oca_logic_ident ~loc ttt)
      else
        ptyp_constr
          ~loc
          (Located.mk ~loc (Lident (fix_tname id)))
          (List.map ~f:helper args)
    | { ptyp_desc = Ptyp_constr (({ txt = Ldot (prefix, id) } as lident), args) } ->
      if ccompositional
      then helper typ
      else if is_old ()
      then (
        let ttt = ptyp_constr ~loc lident (List.map ~f:helper args) in
        oca_logic_ident ~loc ttt)
      else
        ptyp_constr
          ~loc
          (Located.mk ~loc (Ldot (prefix, fix_tname id)))
          (List.map ~f:helper args) *)
    | { ptyp_desc = Ptyp_tuple [ l; r ] } ->
      ptyp_constr
        ~loc
        (Located.mk ~loc @@ lident_of_list [ "OCanren"; "Std"; "Pair"; typ_for_kind ])
        (List.map ~f:helper [ l; r ])
    | { ptyp_desc = Ptyp_tuple ps } ->
      oca_logic_ident ~loc (ptyp_tuple ~loc (List.map ~f:helper ps))
    | _ ->
      Location.raise_errorf
        ~loc
        "can't generate %s type: %a"
        (string_of_kind kind)
        Ppxlib.Pprintast.core_type
        typ
  ;;
*)

  let make2 strat wrap_in_logic ~loc typ =
    let (module S : NAME_MANGLER) = strat in
    let open S in
    let rec helper = function
      | [%type: int] as t -> wrap_in_logic ~loc:t.ptyp_loc t
      | t ->
        (match t.ptyp_desc with
         | Ptyp_tuple ps ->
           wrap_in_logic ~loc:t.ptyp_loc @@ ptyp_tuple ~loc (List.map ~f:helper ps)
         (* | Ptyp_constr ({ txt = Ldot (Lident "GT", _) }, []) -> wrap_in_logic ~loc t *)
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
    make2 st wrap_in_logic ~loc typ
  ;;

  (*  ~ccompositional
      ~loc
      (fun ~loc t -> ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ])
      Reify
      typ *)

  (*   let gtypify_exn ?(ccompositional = false) ~loc typ =
    make_typ_exn ~ccompositional ~loc (fun ~loc:_ t -> t) Prj_exn typ
  ;; *)
  let gtypify_exn ~loc tname typ =
    let st = make_new_mangler Prj_exn tname in
    let wrap_in_logic ~loc:_ t = t in
    make2 st wrap_in_logic ~loc typ
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
    [%expect
      {| (int OCanren.logic, int OCanren.logic) OCanren.Std.Pair.logic Std.List.logic |}];
    ()
  ;;
end

(* TODO(Kakadu): merge to ppx_distrib_expander *)
let make_fmapt_body ~loc gmap_expr count =
  let names : string list =
    List.init ~len:count ~f:(fun _ -> gen_symbol ~prefix:"f" ())
  in
  let add_funs rhs =
    List.fold_right
      names
      ~f:(fun name acc ->
        [%expr fun [%p ppat_var ~loc (Located.mk ~loc name)] -> [%e acc]])
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

let reifier_of_core_type ~loc kind =
  let base_reifier, reifier_name = unwrap_kind ~loc kind in
  let rec helper typ =
    (* Format.printf "reifier_of_core_type '%a'\n" Pprintast.core_type typ; *)
    let loc = typ.ptyp_loc in
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (Lident "GT", "list") }, xs) } ->
      Exp.apply
        ~loc
        (pexp_ident
           ~loc
           (Located.mk ~loc (lident_of_list [ "OCanren"; "Std"; "List"; reifier_name ])))
        (List.map xs ~f:helper)
    | [%type: GT.string]
    | [%type: string]
    | [%type: GT.bool]
    | [%type: bool]
    | [%type: GT.int]
    | [%type: int] -> base_reifier
    | { ptyp_desc = Ptyp_var s } -> pexp_ident ~loc (Located.mk ~loc (lident s))
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
    | { ptyp_desc = Ptyp_constr ({ txt = Lident tname }, args) } when is_new () ->
      let reifier = Printf.sprintf "%s_%s" tname reifier_name in
      pexp_apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (lident reifier)))
        (List.map args ~f:(fun t -> Nolabel, helper t))
    | { ptyp_desc = Ptyp_constr ({ txt = Ldot (Lident m, tname) }, args) } when is_new ()
      ->
      let reifier = Printf.sprintf "%s_%s" tname reifier_name in
      pexp_apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (Ldot (lident m, reifier))))
        (List.map args ~f:(fun t -> Nolabel, helper t))
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
    | { ptyp_desc = Ptyp_constr ({ txt = Lident tname }, args) } when is_new () ->
      let reifier_name = Printf.sprintf "%s_%s" tname reifier_name in
      pexp_apply
        ~loc
        (pexp_ident ~loc (Located.mk ~loc (Lident reifier_name)))
        (List.map args ~f:(fun t -> Nolabel, helper t))
      |> fun e ->
      { e with
        pexp_attributes =
          [ attribute ~loc ~name:(Located.mk ~loc "xxx") ~payload:(PStr []) ]
      }
    | { ptyp_desc = Ptyp_tuple [ l; r ] } ->
      Exp.apply
        ~loc
        (pexp_ident
           ~loc
           (Located.mk
              ~loc
              (Ldot (Ldot (Ldot (Lident "OCanren", "Std"), "Pair"), reifier_name))))
        [ helper l; helper r ]
    | { ptyp_desc = Ptyp_tuple ps } ->
      let gmap_expr =
        let fnames = List.mapi ps ~f:(fun i _ -> Printf.sprintf "f%d" i) in
        let subj_pat =
          ppat_tuple
            ~loc
            (List.map fnames ~f:(fun name ->
               ppat_var ~loc (Located.sprintf ~loc "%ss" name)))
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
                Myhelpers.Exp.apply
                  ~loc
                  [%expr fmapt]
                  (List.map rnames ~f:(Exp.lident ~loc))
              in
              match kind with
              | Prj_exn -> [%expr OCanren.prj_exn <..> chain [%e call_to_fmapt]]
              | Reify ->
                [%expr
                  OCanren.reify
                  <..> chain
                         (OCanren.Reifier.zed
                            (OCanren.Reifier.rework ~fv:[%e call_to_fmapt]))]])]
      in
      Myhelpers.Exp.apply
        ~loc
        (Myhelpers.Exp.funs ~loc body rnames)
        (List.map ~f:helper ps)
    | _ ->
      failwiths
        ~loc
        "Generation of compositional reifier is not yet supported for '%a'"
        Pprintast.core_type
        typ
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
        [%expr fun [%p ppat_var ~loc (Located.mk ~loc (mk_arg_reifier name))] -> [%e acc]])
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
    | Ptyp_constr (_, _args) -> helper ~loc manifest
    | Ptyp_tuple [ l; r ] ->
      let _, reifier_name = unwrap_kind ~loc kind in
      Exp.apply
        ~loc
        (Exp.ident ~loc @@ lident_of_list [ "OCanren"; "Std"; "Pair"; reifier_name ])
        [ helper ~loc l; helper ~loc r ]
    | Ptyp_tuple _ -> helper ~loc manifest
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

let make_reifier ~loc m tdecl =
  make_reifier_composition
    Reify
    ~typ:
      (if List.is_empty tdecl.ptype_params
      then
        Some [%type: (_, [%t ltypify_exn ~loc tdecl.ptype_name.txt m]) OCanren.Reifier.t]
      else None)
    ~pat:
      (ppat_var ~loc (Located.mk ~loc @@ Format.sprintf "reify_%s" tdecl.ptype_name.txt))
    tdecl
;;

let make_prj ~loc m tdecl =
  make_reifier_composition
    Prj_exn
    ~typ:
      (if List.is_empty tdecl.ptype_params
      then
        Some [%type: (_, [%t gtypify_exn tdecl.ptype_name.txt ~loc m]) OCanren.Reifier.t]
      else None)
    ~pat:
      (ppat_var
         ~loc
         (Located.mk ~loc @@ Format.sprintf "prj_exn_%s" tdecl.ptype_name.txt))
    tdecl
;;

let process1 tdecl =
  let loc = tdecl.ptype_loc in
  match tdecl.ptype_manifest with
  | Some m ->
    (* TODO(Kakadu): find a way not to pass both manifest and type declration *)
    [ make_reifier ~loc m tdecl; make_prj ~loc m tdecl ]
  | None -> failwiths ~loc "no manifest"
;;
