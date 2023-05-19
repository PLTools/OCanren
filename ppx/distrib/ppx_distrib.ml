(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2023
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
let filter_out_gt_attributes tdecls = tdecls
(* let helper attrs =
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
      List.map tdecls ~f:(function { ptype_attributes; _ } as g ->
          { g with ptype_attributes = helper ptype_attributes }) *)

let knot_reifiers ~loc ?(kind = Reify_impl.Prj_exn) reifiers base_decls =
  (* Format.eprintf "%s%d\n%!" __FILE__ __LINE__; *)
  assert (List.length reifiers = List.length base_decls);
  match reifiers with
  | [] ->
      (* Format.eprintf "%s%d\n%!" __FILE__ __LINE__; *)
      []
  | [ h ] ->
      let open Ppxlib.Ast_builder.Default in
      let open Ppx_distrib_expander in
      (*       let nameR =
        match kind with
        | Reify_impl.Reify -> "reify"
        | Prj_exn -> "prj_exn"
      in *)
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
    (* let map3' ~f p =
      p |> map2' ~f:(fun loc a b -> loc, a, b) |> map2 ~f:(fun (loc, a, b) c -> f loc a b c)
    in*)
    let map4 ~f p = p |> map3 ~f:(fun a b c -> a, b, c) |> map2 ~f:(fun (a, b, c) d -> f a b c d) in
    (*     let map5 ~f p =
      p |> map3 ~f:(fun a b c -> a, b, c) |> map3 ~f:(fun (a, b, c) d e -> f a b c d e)
    in
    let map4' ~f p =
      p
      |> map3' ~f:(fun loc a b c -> loc, a, b, c)
      |> map2 ~f:(fun (loc, a, b, c) d -> f loc a b c d)
    in *)
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
    let generate ~loc is_rec type_pairs_list =
      let open Ppx_distrib_expander in
      let items =
        let rez = process_main ~loc is_rec type_pairs_list in
        (* Format.eprintf "after process_main %a\n%!" pp_attributes rez.ground.ptype_attributes; *)
        List.concat
          [ [ pstr_type ~loc Nonrecursive [ rez.t ] ]
          ; [ pstr_type ~loc is_rec [ rez.ground ] ]
          ; [ pstr_type ~loc is_rec [ rez.logic ] ]
          ; [ pstr_type ~loc is_rec [ rez.injected ] ]
          ; [ pstr_value ~loc Nonrecursive [ rez.fmapt ] ]
          ; knot_reifiers ~loc ~kind:Prj_exn [ rez.prj_exn ] [ rez.ground ]
          ; knot_reifiers ~loc ~kind:Reify [ rez.reify ] [ rez.ground ]
          ; rez.other
          ]
      in
      pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items))
    in
    let make_extension name =
      Extension.declare
        name
        Extension.Context.Structure_item
        (classify_tdecl ())
        (fun ~loc ~path:_ -> function
        | Other (_, []) -> failwith "Not supported"
        | Other (is_rec, [ ({ ptype_kind = Ptype_abstract; ptype_manifest = Some _ } as tdecl) ]) ->
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
              pstr_type ~loc is_rec [ tdecl ]
              :: pstr_type ~loc is_rec [ ltyp_decl ]
              :: pstr_type ~loc is_rec [ injected_typ_decl ]
              :: Reify_impl.process1 tdecl
            in
            pstr_include ~loc (include_infos ~loc (pmod_structure ~loc stru))
        | Other (is_rec, [ ({ ptype_manifest = Some _ } as tdecl) ]) ->
            (* let tdecl = { tdecl with ptype_attributes } in
                 let open Ppxlib.Ast_builder.Default in
                 let ltyp_decl =
                   let open Ppx_distrib_expander in
                   let abbrev =
                     ltypify_exn
                       ~loc
                       (make_logic_strat_2 tdecl)
                       (Stdlib.Option.get tdecl.ptype_manifest)
                   in
                   let (module S : STRAT) = make_strat () in
                   { tdecl with
                     ptype_name = S.logic_typ_name tdecl
                   ; ptype_manifest = Some abbrev
                   }
                 in
                 let stru =
                   pstr_type ~loc is_rec [ tdecl ]
                   :: pstr_type ~loc is_rec [ ltyp_decl ]
                   :: Reify_impl.process1 tdecl
                 in
                 pstr_include ~loc (include_infos ~loc (pmod_structure ~loc stru)) *)
            assert false
        (*         | Other (is_rec, [ ({ ptype_manifest = None; ptype_kind = Ptype_record labs } as tdecl) ])
          ->
            let full_t, normal_t = Prepare_fully_abstract.run loc [tdecl] in
            generate ~loc full_t is_rec normal_t [] *)
        (* alias of record *)
        (* let type_pairs_list =
                   Prepare_fully_abstract.run loc [ { tdecl with ptype_attributes } ]
                 in
                 generate ~loc is_rec type_pairs_list *)
        (*  pstr_type
              ~loc
              Nonrecursive
              [ { tdecl with
                  ptype_attributes =
                    attribute_of_warning loc "Not supported" :: tdecl.ptype_attributes
                }
              ] *)
        | Other (is_rec, [ ({ ptype_manifest = None; ptype_kind = Ptype_record _ } as tdecl) ])
        | Other (is_rec, [ tdecl ]) ->
            let fuly, ground =
              match Prepare_fully_abstract.run loc [ tdecl ] with
              | [ p ] -> p
              | _ -> assert false
            in
            let rez = Ppx_distrib_expander.process_main ~loc is_rec (fuly, ground) in
            let open Ppxlib.Ast_builder.Default in
            let items =
              List.concat
                [ [ pstr_type ~loc Nonrecursive [ rez.t ] ]
                ; [ pstr_type ~loc is_rec [ rez.ground ] ]
                ; [ pstr_type ~loc is_rec [ rez.logic ] ]
                ; [ pstr_type ~loc is_rec [ rez.injected ] ]
                ; [ pstr_value ~loc Nonrecursive [ rez.fmapt ] ]
                ; knot_reifiers ~loc ~kind:Prj_exn [ rez.prj_exn ] [ ground ]
                ; knot_reifiers ~loc ~kind:Reify [ rez.reify ] [ ground ]
                ; rez.other
                ]
            in
            pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items))
        | Other (is_rec, (_ :: _ :: _ as tdecls)) ->
            let full_and_ground_list = Prepare_fully_abstract.run loc tdecls in
            let rez =
              List.fold_left
                ~f:(fun acc (full, g) ->
                  let rez = Ppx_distrib_expander.process_main ~loc is_rec (full, g) in
                  Ppx_distrib_expander.cons_results rez acc)
                ~init:(Ppx_distrib_expander.empty_rez [] [] [])
                full_and_ground_list
            in
            (* Format.eprintf "%s%d\n%!" __FILE__ __LINE__; *)
            let open Ppxlib.Ast_builder.Default in
            let items =
              let gt_attributes = (List.hd (List.rev tdecls)).ptype_attributes in
              let fully_abstract_types = List.map full_and_ground_list ~f:fst in
              List.concat
                [ List.map
                    ~f:(fun x ->
                      pstr_type ~loc is_rec [ { x with ptype_attributes = gt_attributes } ])
                    rez.t
                ; [ pstr_type ~loc is_rec (filter_out_gt_attributes rez.ground) ]
                ; [ pstr_type ~loc is_rec (filter_out_gt_attributes rez.logic) ]
                ; [ pstr_type ~loc is_rec rez.injected ]
                ; List.map rez.fmapt ~f:(fun vb -> pstr_value ~loc Nonrecursive [ vb ])
                ; knot_reifiers ~loc ~kind:Prj_exn rez.prj_exn fully_abstract_types
                ; knot_reifiers ~loc ~kind:Reify rez.reify fully_abstract_types
                ; rez.other
                ]
            in
            pstr_include ~loc (include_infos ~loc (pmod_structure ~loc items))
        | Explicit
            ( (attributes1, (params1, kind1, private1, manifest1))
            , (rec_2, (params2, kind2, private2, manifest2))
            , other_decls ) ->
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
            generate ~loc rec_2 (base_tdecl, spec_td))
    in
    [ make_extension "distrib"; make_extension "ocanren_inject" ]
  in
  Ppxlib.Driver.register_transformation ~extensions "distrib"
;;

let () =
  Ppxlib.Driver.add_arg
    "-new-typenames"
    (Stdlib.Arg.Unit (fun () -> Reify_impl.config.naming_style <- New_naming))
    ~doc:" Doc here"
;;
