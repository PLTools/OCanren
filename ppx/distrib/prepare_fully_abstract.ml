(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2024
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)
open Ppxlib
open Printf
open Parsetree
open Location
open Myhelpers
module TypeNameMap = Map.Make (String)

module FoldInfo = struct
  type item =
    { param_name : string
    ; rtyp : core_type (** related ground type *)
    ; ltyp : core_type (** related logic type *)
    }

  exception ItemFound of item

  type t = item list

  let param_for_rtyp typ ts =
    let typ_repr =
      Pprintast.core_type Format.str_formatter typ;
      Format.flush_str_formatter ()
    in
    try
      List.iter
        (fun i ->
          let new_repr =
            Pprintast.core_type Format.str_formatter i.rtyp;
            Format.flush_str_formatter ()
          in
          if new_repr = typ_repr then raise (ItemFound i))
        ts;
      None
    with
    | ItemFound i -> Some i
  ;;

  let map ~f (xs : t) = List.map f xs
  let empty = []
  let is_empty : t -> bool = ( = ) []

  let extend param_name rtyp ltyp ts =
    (*      printf "extending by `%s`\n%!" param_name;*)
    { param_name; rtyp; ltyp } :: ts
  ;;
end

let str_type_ = Ast_helper.Str.type_

open Myhelpers

(* convert type to fully-abstract one *)
let abstracting_internal_type ~loc typ (n, map, args) =
  let open Ppxlib.Ast_builder.Default in
  match typ with
  | [%type: _] -> assert false
  | { ptyp_desc = Ptyp_var _; _ } -> n, map, typ :: args
  | arg ->
      (match FoldInfo.param_for_rtyp arg map with
      | Some { param_name } -> n, map, ptyp_var ~loc param_name :: args
      | None ->
          let new_name = sprintf "a%d" n in
          n + 1, FoldInfo.extend new_name arg arg map, ptyp_var ~loc new_name :: args)
;;

let on_variant_ctors ctors =
  List.fold_right
    (fun cd (n, acc_map, cs) ->
      let acc = n, acc_map, [] in
      let loc = cd.pcd_loc in
      match cd.pcd_args with
      | Pcstr_tuple tt ->
          let n, map2, new_args = List.fold_right (abstracting_internal_type ~loc) tt acc in
          let new_args = Pcstr_tuple new_args in
          n, map2, { cd with pcd_args = new_args } :: cs
      | Pcstr_record lds ->
          let typs = List.map (fun ldt -> ldt.pld_type) lds in
          let n, map2, new_args = List.fold_right (abstracting_internal_type ~loc) typs acc in
          let new_args =
            Pcstr_record (List.map2 (fun ld t -> { ld with pld_type = t }) lds new_args)
          in
          n, map2, { cd with pcd_args = new_args } :: cs)
    ctors
    (0, FoldInfo.empty, [])
;;

exception Abstrct_type_found

module String_map = Map.Make (String)

let collect_fully_abstracts tdecls =
  List.fold_left
    (fun acc tdecl ->
      match tdecl.ptype_kind, tdecl.ptype_manifest with
      | Ptype_abstract, None -> raise Abstrct_type_found
      | Ptype_abstract, Some _ -> acc
      | Ptype_variant cds, _ ->
          let _, mapa, cs = on_variant_ctors cds in
          String_map.add
            tdecl.ptype_name.txt
            (mapa, { tdecl with ptype_kind = Ptype_variant cs })
            acc)
    String_map.empty
    tdecls
;;

let prepare_names tdecl =
  match Reify_impl.(config.naming_style) with
  | Reify_impl.New_naming -> tdecl.ptype_name.txt ^ "_fuly", tdecl.ptype_name.txt
  | Old_naming ->
      if String.equal tdecl.ptype_name.txt "t"
      then
        failwiths
          ~loc:tdecl.ptype_loc
          "Don't use type name 't'. We are going to generate fully abstract type, and the names \
           will clash.";
      "t", tdecl.ptype_name.txt
;;

let apply_collected_info info tdecls =
  List.fold_left
    (fun (ful_abstrs, rec_decls) tdecl ->
      match String_map.find tdecl.ptype_name.txt info with
      | exception Not_found -> assert false
      | mapa, td ->
          let full_t_name, ground_name = prepare_names tdecl in
          let new_fully = td in
          let new_rec =
            let default_params = tdecl.ptype_params |> List.map fst in
            let extra_params = FoldInfo.map mapa ~f:(fun fi -> fi.FoldInfo.rtyp) in
            { td with
              ptype_params = tdecl.ptype_params
            ; ptype_name = { tdecl.ptype_name with txt = ground_name }
            ; ptype_kind = Ptype_abstract
            ; ptype_manifest =
                Some
                  (let open Ppxlib.Ast_builder.Default in
                   let loc = td.ptype_loc in
                   ptyp_constr
                     ~loc
                     (Located.mk ~loc @@ Lident full_t_name)
                     (default_params @ extra_params))
            }
          in
          new_fully :: ful_abstrs, new_rec :: rec_decls)
    ([], [])
    tdecls
;;

let run loc tdecls =
  let info = collect_fully_abstracts tdecls in
  apply_collected_info info tdecls
;;
(*
let run loc tdecls =
  assert (
    match tdecls with
    | [] -> false
    | _ -> true);
  let add_extra_attributes =
    (* Usually deriving attribute is attached to the last typdeclaration *)
    let last, prefix =
      let r = List.rev tdecls in
      List.hd r, List.tl r
    in
    if
      List.for_all
        (function
          | { ptype_attributes = [] } -> true
          | _ -> false)
        prefix
    then fun _ -> last.ptype_attributes
    else Fun.id
  in
  let open Ppxlib.Ast_builder.Default in
  (* let tdecl =
       { tdecl with
         ptype_attributes =
           List.filter
             (fun a -> a.attr_name.Location.txt <> "put_distrib_here")
             tdecl.ptype_attributes
       }
     in *)
  let f acc tdecl =
    let mapa, full_t =
      match tdecl.ptype_kind with
      | Ptype_open -> failwiths ~loc "%s: open types are not supported" __FUNCTION__
      | Ptype_abstract ->
          (match tdecl.ptype_manifest with
          | None -> failwiths ~loc "%s: abstract  types are not supported" __FUNCTION__
          | Some t -> FoldInfo.empty, tdecl)
      | Ptype_variant ctors ->
          on_variant_ctors ctors
          |> fun (_, mapa, cs) -> mapa, { tdecl with ptype_kind = Ptype_variant cs }
      | Ptype_record fields ->
          List.fold_right
            (fun field (n, map, args) ->
              let typ = field.pld_type in
              let upd_field typ = { field with pld_type = typ } in
              match typ with
              | [%type: _] -> assert false
              | { ptyp_desc = Ptyp_var _; _ } -> n, map, field :: args
              | arg ->
                  (match FoldInfo.param_for_rtyp arg map with
                  | Some { param_name } -> n, map, upd_field (ptyp_var ~loc param_name) :: args
                  | None ->
                      let new_name = sprintf "a%d" n in
                      ( n + 1
                      , FoldInfo.extend new_name arg arg map
                      , upd_field (ptyp_var ~loc new_name) :: args )))
            fields
            (0, FoldInfo.empty, [])
          |> fun (_, mapa, fields) -> mapa, { tdecl with ptype_kind = Ptype_record fields }
    in
    let make_simple_arg x = x, (Asttypes.NoVariance, Asttypes.NoInjectivity) in
    let full_t_name, ground_name = prepare_names tdecl in
    let full_t =
      { full_t with
        ptype_name = { full_t.ptype_name with txt = full_t_name }
      ; ptype_attributes = add_extra_attributes full_t.ptype_attributes
      ; ptype_params =
          full_t.ptype_params
          @ FoldInfo.map mapa ~f:(fun { FoldInfo.param_name } ->
              make_simple_arg (ptyp_var ~loc param_name))
      }
    in
    (* now we need to add some parameters if we collected ones *)
    let abbrev_typ =
      let result_type =
        if FoldInfo.is_empty mapa
        then (
          let default_params = tdecl.ptype_params |> List.map fst in
          { tdecl with
            ptype_manifest =
              Some
                (ptyp_constr ~loc (Located.mk ~loc @@ Lident full_t.ptype_name.txt) default_params)
          ; ptype_kind = Ptype_abstract
          })
        else (
          let default_params = tdecl.ptype_params |> List.map fst in
          let extra_params = FoldInfo.map mapa ~f:(fun fi -> fi.FoldInfo.rtyp) in
          { full_t with
            ptype_params = tdecl.ptype_params
          ; ptype_name = { tdecl.ptype_name with txt = ground_name }
          ; ptype_kind = Ptype_abstract
          ; ptype_manifest =
              Some
                (ptyp_constr
                   ~loc
                   (Located.mk ~loc @@ Lident full_t.ptype_name.txt)
                   (default_params @ extra_params))
          })
      in
      result_type
    in
    (full_t, abbrev_typ) :: acc
  in
  List.fold_left f [] tdecls
;;
*)
