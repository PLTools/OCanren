(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2024
 *   Dmitrii Kosarev aka Kakadu, Petr Lozov
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx
open Ppxlib.Ast_builder.Default
open Ppxlib.Ast_helper
open Printf
module Format = Stdlib.Format
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
      let (_ : int) = Stdlib.Sys.command _cmd in
      ())
    fmt
;;

let ( @@ ) = Stdlib.( @@ )
let nolabel = Asttypes.Nolabel

let mangle_construct_name name =
  let low =
    String.mapi
      ~f:(function
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

module type STRAT = sig
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

let make_logic_strat_2 tdecl =
  let module I = struct
    let sort = `Reify
    let self_typ_name = if Reify_impl.is_new () then tdecl.ptype_name.txt ^ "_logic" else "logic"
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
        [ (let ful_lident = if Reify_impl.is_new () then tdecl.ptype_name.txt ^ "_fuly" else "t" in
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

    let mangle_lident = Reify_impl.create_lident_mangler `Reify
  end in
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
        [ (let ful_lident = if Reify_impl.is_new () then tdecl.ptype_name.txt ^ "_fuly" else "t" in
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

    let mangle_lident = Reify_impl.create_lident_mangler `Injected
  end in
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
          (* In principle, special treating of pairs is not required,
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
                (Located.mk
                   ~loc
                   (lident_of_list
                      [ "OCanren"
                      ; (match S.sort with
                        | `Injected -> "ilogic"
                        | _ -> stdname_of_sort S.sort)
                      ]))
                [ ptyp_tuple ~loc (List.map ~f:helper ps) ]
          | Ptyp_constr ({ txt = Ldot (Lident "GT", _) }, []) -> oca_logic_ident ~loc:t.ptyp_loc t
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
          (Located.mk ~loc @@ lident_of_list [ "OCanren"; "Std"; "Pair"; stdname_of_sort S.sort ])
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
    make_typ_exn ~loc (fun ~loc t -> ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ])
  ;;

  let injectify ~loc =
    let oca_logic_ident ~loc = Located.mk ~loc (lident_of_list [ "OCanren"; "ilogic" ]) in
    make_typ_exn ~loc (fun ~loc t -> ptyp_constr ~loc (oca_logic_ident ~loc:t.ptyp_loc) [ t ])
  ;;

  let gtypify_exn ~loc = make_typ_exn ~loc (fun ~loc:_ t -> t)
end

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
  end in
  (module M : STRAT)
;;

type ('a, 'b, 'ri) the_result =
  { t : 'a (** fully abstract type definitions *)
  ; ground : 'a
  ; logic : 'a
  ; injected : 'a
  ; fmapt : 'b
  ; prj_exn : 'ri
  ; reify : 'ri
  ; other : structure_item list
  ; other_sigs : signature_item list
  }

let cons_results { t; ground; logic; injected; fmapt; prj_exn; reify; other; other_sigs } next =
  { t = t :: next.t
  ; ground = ground :: next.ground
  ; logic = logic :: next.logic
  ; injected = injected :: next.injected
  ; fmapt = fmapt :: next.fmapt
  ; prj_exn = prj_exn :: next.prj_exn
  ; reify = reify :: next.reify
  ; other = other @ next.other
  ; other_sigs = other_sigs @ next.other_sigs
  }
;;

let empty_rez va vb ri =
  { t = va
  ; ground = va
  ; logic = va
  ; injected = va
  ; fmapt = vb
  ; reify = ri
  ; prj_exn = ri
  ; other = []
  ; other_sigs = []
  }
;;

module Reifier_info = struct
  type t =
    { typ : Ppxlib.core_type option
    ; body : expression
    ; name : string
    ; decl : type_declaration
    }
end

let remove_deriving_gmap attr =
  let helper stri =
    match stri with
    | [%expr gt ~options:[%e? opts]] ->
        (match opts.pexp_desc with
        | Pexp_record (labs, other) ->
            let labs =
              List.filter labs ~f:(fun (lab, _) ->
                  match lab.txt with
                  | Lident "gmap" -> false
                  | _ -> true)
            in
            let loc = opts.pexp_loc in
            (match labs with
            | [] -> [%expr gt]
            | _ -> [%expr gt ~options:[%e pexp_record ~loc:opts.pexp_loc labs other]])
        | _ -> stri)
    | _ -> stri
  in
  match attr.attr_payload with
  | PStr [%str [%e? e]] ->
      (* notify "%s %d" __FILE__ __LINE__; *)
      Some { attr with attr_payload = PStr [ pstr_eval ~loc:attr.attr_loc (helper e) [] ] }
  | _ -> None
;;

let filter_out_gmap_attr attrs =
  List.concat_map attrs ~f:(fun attr ->
      match attr.attr_name.txt with
      | "distrib" -> []
      | "deriving" ->
          remove_deriving_gmap attr
          |> (function
          | Some x -> [ x ]
          | None -> [])
      | _ -> [ attr ])
;;

let decorate_with_attributes tdecl attrs = { tdecl with ptype_attributes = attrs }
let mk_arg_reifier s = sprintf "r%s" s

let make_reifier_gen ~kind is_rec tdecl : Reifier_info.t =
  (* let names = extract_names tdecl.ptype_params in *)
  let loc = tdecl.ptype_loc in
  let _pat, base_reifier, name =
    match kind with
    | Reify_impl.Reify -> [%pat? reify], [%expr OCanren.reify], "reify"
    | Prj_exn -> [%pat? prj_exn], [%expr OCanren.prj_exn], "prj_exn"
  in
  let pat_name =
    if Reify_impl.is_old ()
    then Reify_impl.string_of_kind kind
    else Printf.sprintf "%s_%s" tdecl.ptype_name.txt (Reify_impl.string_of_kind kind)
  in
  match tdecl.ptype_manifest with
  | Some manifest ->
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
            let reifier = Exp.ident ~loc (lident_of_list [ "OCanren"; "Std"; "Pair"; name ]) in
            [%expr [%e reifier] [%e helper l] [%e helper r]]
        | { ptyp_desc = Ptyp_tuple _ } -> failwiths ~loc "Not implemented"
        | { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, name) }, args) } when Reify_impl.is_new () ->
            (* Myhelpers.notify "%s %d" __FILE__ __LINE__; *)
            let tname = Format.sprintf "%s_%s" name (Reify_impl.string_of_kind kind) in
            let rhs = pexp_ident ~loc (Located.mk ~loc (Ldot (m, tname))) in
            List.fold_left ~init:rhs args ~f:(fun acc x ->
                pexp_apply ~loc acc [ nolabel, helper x ])
        | { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, _) }, args) } ->
            let rhs = pexp_ident ~loc (Located.mk ~loc (Ldot (m, name))) in
            List.fold_left ~init:rhs args ~f:(fun acc x ->
                pexp_apply ~loc acc [ nolabel, helper x ])
        | { ptyp_desc = Ptyp_constr ({ txt = Lident name }, args) } when Reify_impl.is_new () ->
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
              let f =
                if Reify_impl.is_new ()
                then Exp.lident ~loc (sprintf "%s_fmapt" tdecl.ptype_name.txt)
                else [%expr fmapt]
              in
              pexp_apply ~loc f (List.map args ~f:(fun t -> Nolabel, helper t))
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
            failwiths ~loc:manifest.ptyp_loc "Not supported %s %d" Stdlib.__FILE__ Stdlib.__LINE__
      in
      { Reifier_info.typ = None; body = body (); name = pat_name; decl = tdecl }
  | None -> assert false
;;

let reifier_for_fully_abstract ~kind tdecl =
  let loc = tdecl.ptype_loc in
  let _, base_reifier, _ =
    match kind with
    | Reify_impl.Reify -> [%pat? reify], [%expr OCanren.reify], "reify"
    | Prj_exn -> [%pat? prj_exn], [%expr OCanren.prj_exn], "prj_exn"
  in
  let fmapt =
    if Reify_impl.is_new ()
    then pexp_ident ~loc (Located.mk ~loc (Lident (sprintf "%s_fmapt" tdecl.ptype_name.txt)))
    else [%expr fmapt]
  in
  let pat_name =
    if Reify_impl.is_old ()
    then Reify_impl.string_of_kind kind
    else Printf.sprintf "%s_%s" tdecl.ptype_name.txt (Reify_impl.string_of_kind kind)
  in
  let body () =
    let fmapt =
      pexp_apply
        ~loc
        fmapt
        (List.map tdecl.ptype_params ~f:(fun (t, _) ->
             match t.ptyp_desc with
             | Ptyp_var v -> Nolabel, pexp_ident ~loc (Located.mk ~loc (Lident (sprintf "f%s" v)))
             | _ -> assert false))
    in
    let self_pat = [%pat? _] in
    [%expr
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun [%p self_pat] ->
          [%e base_reifier]
          <..> chain
                 [%e
                   match kind with
                     | Reify -> [%expr OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:[%e fmapt])]
                     | Prj_exn -> fmapt])]
  in
  { Reifier_info.typ = None; body = body (); name = pat_name; decl = tdecl }
;;

let pp_attributes ppf attrs =
  List.iteri attrs ~f:(fun i -> function
    | { attr_payload = PStr stru } -> Format.fprintf ppf "%d: %a\n%!" i Pprintast.structure stru
    | _ -> Format.fprintf ppf "pprinting is not implemented")
;;

let process_main ~loc rec_ (base_tdecl, tdecl) =
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
            ptyp_constr ~loc:t.ptyp_loc (Located.mk ~loc (Lident "ground")) (List.map ~f:helper ps)
        | Ptyp_constr (lident, ps) -> ptyp_constr ~loc:t.ptyp_loc lident (List.map ~f:helper ps)
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
      ; ptype_attributes = base_tdecl.ptype_attributes
      })
  in
  let ltyp =
    (* let ptype_attributes =
         List.concat_map tdecl.ptype_attributes ~f:(fun attr ->
           match attr.attr_name.txt with
           | "distrib" -> []
           | "deriving" ->
             remove_deriving_gmap attr
             |> (function
             | Some x -> [ x ]
             | None -> [])
           | _ -> [ attr ])
       in *)
    let ptype_name = S.logic_typ_name tdecl in
    let ptype_manifest =
      match tdecl.ptype_manifest with
      | None -> failwiths ~loc:tdecl.ptype_loc "no manifest %s %d" __FILE__ __LINE__
      | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) ->
          Some (ltypify_exn ~loc (make_logic_strat_2 tdecl) typ)
      | t -> t
    in
    { tdecl with ptype_name; ptype_manifest; ptype_attributes = base_tdecl.ptype_attributes }
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
          add_ilogic ~loc @@ ptyp_constr ~loc cname (List.map ~f:helper args) |> fun x -> Some x
      | Some ({ ptyp_desc = Ptyp_constr (_, _) } as typ) -> Some (helper typ)
      | t ->
          (* Format.printf "HERR %s %d\n%!" __FILE__ __LINE__; *)
          t
    in
    let td =
      type_declaration
        ~loc
        ~name
        ~private_:Public
        ~kind:Ptype_abstract
        ~cstrs:[]
        ~params:(List.map param_type_vars ~f:(fun x -> x, (NoVariance, NoInjectivity)))
        ~manifest:ptype_manifest
    in
    td
  in
  let creators =
    if Reify_impl.is_new ()
    then []
    else (
      let name cd = mangle_construct_name cd.pcd_name.txt in
      let make_stri prim_pat add_args rhs =
        [%stri let [%p prim_pat] = [%e add_args [%expr OCanren.inj [%e rhs]]]]
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
      | Ptype_abstract -> []
      | Ptype_open ->
          failwiths
            ~loc:base_tdecl.ptype_loc
            "%s %d Open and abstract types are not supported"
            Stdlib.__FILE__
            Stdlib.__LINE__)
  in
  (* let typ_of_decl decl =
       ptyp_constr
         ~loc
         (Located.mk ~loc (Lident decl.ptype_name.txt))
         (List.map decl.ptype_params ~f:(fun (t, _) ->
              match t.ptyp_desc with
              | Ptyp_var _v -> t
              | _ -> assert false))
     in *)
  let make_fmapt decl =
    let names =
      extract_names (name_type_params_in_td decl).ptype_params
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
    let pat =
      ppat_var
        ~loc
        (if Reify_impl.is_new ()
         then Located.sprintf ~loc "%s_fmapt" tdecl.ptype_name.txt
         else Located.sprintf ~loc "fmapt")
    in
    value_binding ~loc ~pat ~expr
  in
  let add_typ ~kind ri =
    let g =
      match kind with
      | Reify_impl.Reify -> ltyp
      | Prj_exn -> tdecl
    in
    let injected_typ =
      let lident = Located.mk ~loc (Lident ityp.ptype_name.txt) in
      let lident_g = Located.mk ~loc (Lident g.ptype_name.txt) in
      let names = extract_names tdecl.ptype_params in
      let mangle = sprintf "%s_2" in
      let names_2 = List.map names ~f:mangle in
      let rez =
        [%type:
          ( [%t ptyp_constr ~loc lident (List.map names ~f:(ptyp_var ~loc))]
          , [%t ptyp_constr ~loc lident_g (List.map names_2 ~f:(ptyp_var ~loc))] )
          OCanren.Reifier.t]
      in
      List.fold_right names ~init:rez ~f:(fun name acc ->
          [%type:
               ([%t ptyp_var ~loc name], [%t ptyp_var ~loc (mangle name)]) OCanren.Reifier.t
            -> [%t acc]])
    in
    { ri with Reifier_info.typ = Some injected_typ }
  in
  { t = base_tdecl
  ; ground = decorate_with_attributes tdecl base_tdecl.ptype_attributes
  ; logic = ltyp
  ; injected = ityp
  ; fmapt = make_fmapt base_tdecl
  ; prj_exn = add_typ ~kind:Prj_exn (make_reifier_gen ~kind:Prj_exn is_rec tdecl)
  ; reify = add_typ ~kind:Reify (make_reifier_gen ~kind:Reify is_rec tdecl)
  ; other = creators
  ; other_sigs = [] (* TODO: signatures of creators will be added later *)
  }
;;

(* TODO(Kakadu): remember why this is really needed *)
(*
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
*)
(* let prepare_reifiers (rs : Reifier_info.t list) =
        let names = [] (* type params of original decl *) in
        let mk_arg_reifier s = sprintf "r%s" s in
        let add_args tdecl rhs =
          let loc = tdecl.ptype_loc in
          Exp.funs ~loc rhs (List.map ~f:mk_arg_reifier names)
        in
        match rs with
        | [] -> []
        | [ h ] ->
            let loc = h.Reifier_info.decl.ptype_loc in
            let pat = ppat_var ~loc (Located.mk ~loc h.Reifier_info.name) in
            [ value_binding ~loc ~pat ~expr:(add_args h.Reifier_info.decl h.Reifier_info.body) ]
        | rs ->
            let mutual_names = List.map rs ~f:(fun { Reifier_info.decl } -> decl.ptype_name.txt) in
            let mutual_args ~loc e =
              pexp_fun
                ~loc
                nolabel
                None
                (ppat_tuple
                   ~loc
                   (List.map mutual_names ~f:(fun name -> ppat_var ~loc (Located.mk ~loc name))))
                e
            in
            List.concat
              [ List.map rs ~f:(fun { Reifier_info.name; decl; body } ->
                    let loc = decl.ptype_loc in
                    let pat = ppat_var ~loc (Located.mk ~loc name) in
                    value_binding ~loc ~pat ~expr:(mutual_args ~loc @@ add_args decl body))
              ; List.concat_map rs ~f:(fun _ -> [])
              ]
      ;;
*)
