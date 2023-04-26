(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2024
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx

type ground_input =
  rec_flag * ((core_type * (variance * injectivity)) list * type_kind * private_flag * core_type)

type input =
  | Explicit of
      (Ppxlib__Import.attributes
      * ((core_type * (variance * injectivity)) list * type_kind * private_flag * core_type option))
      * ground_input
      * structure_item list
  | Other of (Asttypes.rec_flag * type_declaration list)

let pp_input ppf = function
  | Explicit _ -> Format.fprintf ppf "Explicit"
  | Other _ -> Format.fprintf ppf "Other"
;;

(* For mutual recursion gt only available for regular types. Currently we forbit it at all *)
let filter_out_gt_attributes tdecls =
  let helper attrs =
    List.filter attrs ~f:(fun attr ->
        match attr.attr_name.txt with
        | "deriving" ->
            (match attr.attr_payload with
            | PStr [%str gt] | PStr [%str gt ~options:[%e? _]] -> false
            | _ -> true)
        | _ -> true)
  in
  match tdecls with
  | [] | [ _ ] -> tdecls
  | _ ->
      let len = List.length tdecls in
      List.mapi tdecls ~f:(fun i ->
          if i < len - 1
          then
            function
            | { ptype_attributes; _ } as g -> { g with ptype_attributes = helper ptype_attributes }
          else Fun.id)
;;

let knot_reifiers_sig ~loc reifiers =
  List.concat_map reifiers ~f:(fun ri ->
      let open Ppx_distrib_expander in
      match ri.Reifier_info.typ with
      | None -> []
      | Some t ->
          let open Ppxlib.Ast_builder.Default in
          let name = Located.mk ~loc ri.Reifier_info.name in
          [ psig_value ~loc (value_description ~loc ~name ~prim:[] ~type_:t) ])
;;

let knot_reifiers ~loc ?(kind = Reify_impl.Prj_exn) reifiers base_decls =
  assert (List.length reifiers = List.length base_decls);
  match reifiers with
  | [] -> []
  | [ h ] ->
      let open Ppxlib.Ast_builder.Default in
      let open Ppx_distrib_expander in
      let base_decl = List.hd base_decls in
      let rec_flag =
        (* Without mutual recursion we rely on fixpoint *)
        Nonrecursive
      in
      let expr = h.Reifier_info.body in
      let pat =
        let p = ppat_var ~loc (Located.mk ~loc h.Reifier_info.name) in
        match h.Reifier_info.typ with
        | Some t -> ppat_constraint ~loc p t
        | None -> p
      in
      let expr =
        Myhelpers.Exp.funs
          ~loc
          expr
          (List.map ~f:(Printf.sprintf "r%s") (Myhelpers.extract_names base_decl.ptype_params))
      in
      [ pstr_value ~loc rec_flag [ value_binding ~loc ~pat ~expr ] ]
  | reifiers ->
      let open Ppxlib.Ast_builder.Default in
      let open Ppx_distrib_expander in
      let nameR =
        match kind with
        | Reify_impl.Reify -> "reify"
        | Prj_exn -> "prj_exn"
      in
      let joined = List.combine (List.rev reifiers) base_decls in
      let mangle1name = Printf.sprintf "%s_%s" in
      let knotted_names =
        List.map joined ~f:(function { Ppx_distrib_expander.Reifier_info.decl }, _ ->
            mangle1name decl.ptype_name.txt nameR)
      in
      [ (List.map joined ~f:(fun ({ Reifier_info.decl }, tdecl) ->
             let info =
               reifier_for_fully_abstract ~kind { tdecl with ptype_name = decl.ptype_name }
             in
             let pat =
               ppat_var ~loc (Located.mk ~loc (Printf.sprintf "__%s" decl.ptype_name.txt))
             in
             let expr =
               Myhelpers.Exp.funs
                 ~loc
                 info.body
                 (List.map ~f:(Printf.sprintf "f%s") (Myhelpers.extract_names tdecl.ptype_params))
             in
             value_binding ~loc ~pat ~expr)
        |> fun xs -> [ pstr_value ~loc Recursive xs ])
      ; (let vbs =
           List.map2
             joined
             knotted_names
             ~f:(fun ({ Ppx_distrib_expander.Reifier_info.decl }, _) pat_name ->
               let pat = ppat_var ~loc (Located.mk ~loc pat_name) in
               let expr =
                 let manifest = decl.ptype_manifest |> Stdlib.Option.get in
                 match manifest.ptyp_desc with
                 | Ptyp_constr ({ txt = Lident s }, args) when String.ends_with ~suffix:"_fuly" s ->
                     let tname =
                       "__" ^ String.sub s ~pos:0 ~len:(String.length s - String.length "_fuly")
                     in
                     let args_reifiers =
                       List.map
                         args
                         ~f:
                           (Reify_impl.reifier_of_core_type
                              ~loc
                              ~reifier_for_var:(Printf.sprintf "f%s")
                              kind)
                     in
                     [%expr
                       [%e
                         pexp_apply
                           ~loc
                           (Myhelpers.Exp.lident ~loc tname)
                           (nolabelize args_reifiers)]
                         eta]
                 | _ -> assert false
               in
               let expr =
                 Myhelpers.Exp.funs
                   ~loc
                   [%expr fun eta -> [%e expr]]
                   (List.map ~f:(Printf.sprintf "f%s") (Myhelpers.extract_names decl.ptype_params))
               in
               value_binding ~loc ~pat ~expr)
         in
         pexp_let
           ~loc
           Recursive
           vbs
           (pexp_tuple
              ~loc
              (List.map knotted_names ~f:(fun name ->
                   pexp_ident ~loc (Located.mk ~loc @@ Lident name))))
         |> fun e ->
         [ pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:[%pat? fix] ~expr:e ] ])
      ; List.map knotted_names ~f:(fun knotted ->
            let expr =
              let pat =
                ppat_tuple
                  ~loc
                  (List.map knotted_names ~f:(fun n ->
                       if String.equal knotted n
                       then ppat_var ~loc (Located.mk ~loc "f")
                       else ppat_any ~loc))
              in
              [%expr
                fun eta ->
                  [%e
                    pexp_let
                      ~loc
                      Nonrecursive
                      [ value_binding ~loc ~pat ~expr:[%expr fix] ]
                      [%expr f eta]]]
            in
            pstr_value
              ~loc
              Nonrecursive
              [ value_binding ~loc ~pat:(ppat_var ~loc (Located.mk ~loc knotted)) ~expr ])
      ]
      |> List.concat
      |> pmod_structure ~loc
      |> include_infos ~loc
      |> pstr_include ~loc
      |> fun x -> [ x ]
;;

let classify_tdecl () =
  let pattern =
    let open Ast_pattern in
    let map3 ~f p = p |> map2 ~f:(fun a b -> a, b) |> map2 ~f:(fun (a, b) c -> f a b c) in
    let map4 ~f p = p |> map3 ~f:(fun a b c -> a, b, c) |> map2 ~f:(fun (a, b, c) d -> f a b c d) in
    let rec conde xs =
      match xs with
      | [] -> failwith "bad argument"
      | [ h ] -> h
      | h :: tl -> h ||| conde tl
    in
    let p_fully () =
      (* TODO: check for fully abstractness *)
      as__
      @@ pstr_type
           nonrecursive
           ((type_declaration_attributes __
            @@ (type_declaration
                  ~name:(string "t")
                  ~params:__
                  ~cstrs:nil
                  ~kind:__
                  ~private_:__
                  ~manifest:__
               |> map4 ~f:(fun a b c d -> a, b, c, d)))
           ^:: nil)
      |> map3 ~f:(fun _ a b -> a, b)
    in
    let p_ground () =
      as__
      @@ pstr_type
           __
           ((type_declaration
               ~name:(string "ground")
               ~params:__
               ~cstrs:nil
               ~kind:__
               ~private_:__
               ~manifest:(some __)
            |> map4 ~f:(fun a b c d -> a, b, c, d))
           ^:: nil)
      |> map3 ~f:(fun _ a b -> a, b)
    in
    let p_not_fully_abstract () =
      let single =
        type_declaration_attributes
          __
          (as__
          @@ type_declaration
               ~name:drop
               ~params:drop
               ~cstrs:nil
               ~kind:drop
               ~private_:drop
               ~manifest:drop)
        |> map2 ~f:(fun attrs td -> { td with ptype_attributes = attrs })
      in
      pstr_type __ (many single) ^:: nil
    in
    pstr
      (conde
         [ p_fully () ^:: p_ground () ^:: __ |> map3 ~f:(fun a b c -> Explicit (a, b, c))
         ; p_not_fully_abstract () |> map2 ~f:(fun x y -> Other (x, y))
         ]
         (*************************************************))
  in
  pattern
;;

let%expect_test _ =
  let loc = Location.none in
  let wrap both =
    Ast_pattern.parse
      (classify_tdecl ())
      loc
      ~on_error:(fun () -> assert false)
      (PStr both)
      (Format.printf "%a\n%!" pp_input)
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

open Ppxlib.Ast_builder.Default

let () =
  let extensions =
    let make_extension_gen (type ai a) :
           ctx:a Extension.Context.t
        -> of_tdecl:(loc:location -> rec_flag -> type_declaration list -> ai)
        -> (* TODO: maybe all arguments should return lists of values? *)
           of_value:(loc:location -> rec_flag -> value_binding -> ai list)
        -> group_items:(loc:location -> ai list -> a)
        -> knot_reifiers:
             (   loc:location
              -> ?kind:Reify_impl.kind
              -> Ppx_distrib_expander.Reifier_info.t list
              -> type_declaration list
              -> ai list)
        -> other_stuff:
             (   ( type_declaration list
                 , value_binding list
                 , Ppx_distrib_expander.Reifier_info.t list )
                 Ppx_distrib_expander.the_result
              -> ai list)
        -> string
        -> Extension.t =
     fun ~ctx ~of_tdecl ~of_value ~group_items ~knot_reifiers ~other_stuff name ->
      Extension.declare name ctx (classify_tdecl ()) (fun ~loc ~path:_ -> function
        | Other (_, []) -> failwith "Not supported"
        | Other (is_rec, [ ({ ptype_kind = Ptype_abstract; ptype_manifest = Some _ } as tdecl) ]) ->
            (* type abbreviation *)
            let open Ppxlib.Ast_builder.Default in
            let ltyp_decl =
              let open Ppx_distrib_expander in
              let abbrev =
                ltypify_exn ~loc (make_logic_strat_2 tdecl) (Stdlib.Option.get tdecl.ptype_manifest)
              in
              let (module S : STRAT) = make_strat () in
              { tdecl with ptype_name = S.logic_typ_name tdecl; ptype_manifest = Some abbrev }
            in
            let injected_typ_decl =
              let open Ppx_distrib_expander in
              let abbrev =
                injectify
                  ~loc
                  (make_injected_strat_2 tdecl)
                  (Stdlib.Option.get tdecl.ptype_manifest)
              in
              let (module S : STRAT) = make_strat () in
              { tdecl with
                ptype_name = S.injected_typ_name tdecl
              ; ptype_manifest = Some abbrev
              ; ptype_attributes = []
              }
            in
            let stru =
              List.concat
                [ [ of_tdecl ~loc is_rec [ tdecl ] ]
                ; [ of_tdecl ~loc is_rec [ ltyp_decl ] ]
                ; [ of_tdecl ~loc is_rec [ injected_typ_decl ] (* :: Reify_impl.process1 tdecl *) ]
                ; other_stuff
                    { (Ppx_distrib_expander.empty_rez [] [] []) with
                      other = Reify_impl.process_str tdecl
                    ; other_sigs = Reify_impl.process_sig tdecl
                    }
                ]
            in
            group_items ~loc stru
        | Other (_is_rec, [ { ptype_manifest = Some _ } ]) ->
            (* TODO *)
            failwith "Should not happen, don't remember why"
        | Other (is_rec, [ tdecl ]) ->
            (* ADT or record *)
            let fuly, ground =
              match Prepare_fully_abstract.run loc [ tdecl ] with
              | [ p ] -> p
              | _ -> assert false
            in
            let rez = Ppx_distrib_expander.process_main ~loc is_rec (fuly, ground) in
            let open Ppxlib.Ast_builder.Default in
            let stru =
              List.concat
                [ [ of_tdecl ~loc Nonrecursive [ rez.t ] ]
                ; [ of_tdecl ~loc is_rec [ rez.ground ] ]
                ; [ of_tdecl ~loc is_rec [ rez.logic ] ]
                ; [ of_tdecl ~loc is_rec [ rez.injected ] ]
                ; of_value ~loc Nonrecursive rez.fmapt
                ; knot_reifiers ~loc ~kind:Prj_exn [ rez.prj_exn ] [ ground ]
                ; knot_reifiers ~loc ~kind:Reify [ rez.reify ] [ ground ]
                ; other_stuff Ppx_distrib_expander.(cons_results rez (empty_rez [] [] []))
                ]
            in
            group_items ~loc stru
        | Other (is_rec, (_ :: _ :: _ as tdecls)) ->
            (* Mutual recursion *)
            let full_and_ground_list = Prepare_fully_abstract.run loc tdecls in
            let rez =
              List.fold_left
                ~f:(fun acc (full, g) ->
                  let rez = Ppx_distrib_expander.process_main ~loc is_rec (full, g) in
                  Ppx_distrib_expander.cons_results rez acc)
                ~init:(Ppx_distrib_expander.empty_rez [] [] [])
                full_and_ground_list
            in
            let open Ppxlib.Ast_builder.Default in
            let items =
              let gt_attributes = (List.hd (List.rev tdecls)).ptype_attributes in
              let fully_abstract_types = List.map full_and_ground_list ~f:fst in
              List.concat
                [ List.map
                    ~f:(fun x ->
                      of_tdecl ~loc Nonrecursive [ { x with ptype_attributes = gt_attributes } ])
                    rez.t
                ; [ of_tdecl ~loc is_rec (filter_out_gt_attributes rez.ground) ]
                ; [ of_tdecl ~loc is_rec (filter_out_gt_attributes rez.logic) ]
                ; [ of_tdecl ~loc is_rec rez.injected ]
                ; List.concat_map rez.fmapt ~f:(fun vb -> of_value ~loc Nonrecursive vb)
                ; knot_reifiers ~loc ~kind:Prj_exn rez.prj_exn fully_abstract_types
                ; knot_reifiers ~loc ~kind:Reify rez.reify fully_abstract_types
                ; other_stuff rez
                ]
            in
            group_items ~loc items
        | Explicit
            ( (attributes1, (params1, kind1, private1, manifest1))
            , (rec_2, (params2, kind2, private2, manifest2))
            , _other_decls ) ->
            let open Ppxlib.Ast_builder.Default in
            let base_tdecl =
              let td =
                type_declaration
                  ~loc
                  ~name:(Located.mk ~loc "t")
                  ~params:params1
                  ~cstrs:[]
                  ~private_:private1
                  ~kind:kind1
                  ~manifest:manifest1
              in
              { td with ptype_attributes = attributes1 }
            in
            let spec_td =
              let td =
                type_declaration
                  ~loc
                  ~params:params2
                  ~cstrs:[]
                  ~name:(Located.mk ~loc "ground")
                  ~kind:kind2
                  ~private_:private2
                  ~manifest:(Some manifest2)
              in
              { td with ptype_attributes = attributes1 }
            in
            let is_rec = rec_2 in
            let open Ppx_distrib_expander in
            let items =
              let rez = process_main ~loc is_rec (base_tdecl, spec_td) in
              (* Format.eprintf "after process_main %a\n%!" pp_attributes rez.ground.ptype_attributes; *)
              List.concat
                [ [ of_tdecl ~loc Nonrecursive [ rez.t ] ]
                ; [ of_tdecl ~loc is_rec [ rez.ground ] ]
                ; [ of_tdecl ~loc is_rec [ rez.logic ] ]
                ; [ of_tdecl ~loc is_rec [ rez.injected ] ]
                ; of_value ~loc Nonrecursive rez.fmapt
                ; knot_reifiers ~loc ~kind:Prj_exn [ rez.prj_exn ] [ rez.ground ]
                ; knot_reifiers ~loc ~kind:Reify [ rez.reify ] [ rez.ground ]
                ; other_stuff Ppx_distrib_expander.(cons_results rez (empty_rez [] [] []))
                ]
            in
            group_items ~loc items)
    in
    let make_extension_str =
      make_extension_gen
        ~ctx:Extension.Context.Structure_item
        ~of_value:(fun ~loc flg v -> [ pstr_value ~loc flg [ v ] ])
        ~of_tdecl:Ast_builder.Default.pstr_type
        ~group_items:(fun ~loc stru ->
          pstr_include ~loc (include_infos ~loc (pmod_structure ~loc stru)))
        ~knot_reifiers
        ~other_stuff:(fun rez -> rez.other)
    in
    let make_extension_sig =
      make_extension_gen
        ~ctx:Extension.Context.Signature_item
        ~of_value:(fun ~loc:_ _flg _v ->
          (* We usually pu fmap here, but it should not be in the signature *)
          [])
        ~of_tdecl:Ast_builder.Default.psig_type
        ~group_items:(fun ~loc stru ->
          psig_include ~loc (include_infos ~loc (pmty_signature ~loc stru)))
        ~knot_reifiers:(fun ~loc ?kind:_ reifiers _ -> knot_reifiers_sig ~loc reifiers)
        ~other_stuff:(fun rez -> rez.other_sigs)
    in
    [ make_extension_str "distrib"
    ; make_extension_sig "ocanren_inject"
    ; make_extension_str "ocanren_inject"
    ]
  in
  Ppxlib.Driver.register_transformation ~extensions "distrib"
;;

let () =
  Ppxlib.Driver.add_arg
    "-new-typenames"
    (Stdlib.Arg.Unit (fun () -> Reify_impl.config.naming_style <- New_naming))
    ~doc:" Doc here"
;;
