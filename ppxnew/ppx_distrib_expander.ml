(*
 * OCanren PPX
 * Copyright (C) 2016-2020
 *   Dmitrii Kosarev aka Kakadu, Petr Lozov
 * St.Petersburg State University, JetBrains Research
 *)

open Base
open Ppxlib
open Ppxlib.Ast_builder.Default
open Ppxlib.Ast_helper
open Printf

let (@@) = Caml.(@@)

module Naming = struct
  let fabst_name = sprintf "g%s"
  let functor_name = sprintf "For_%s"
end


module TypeNameMap = Map.M(String)

module FoldInfo = struct
  (* using fields of structure below we can generate ground type and the logic type *)
  type item = { param_name:string; rtyp: core_type; ltyp: core_type }
  exception ItemFound of item
  type t = item list

  let param_for_rtyp typ ts =
    let typ_repr =
      Pprintast.core_type Caml.Format.str_formatter typ;
      Caml.Format.flush_str_formatter ()
    in
    try List.iter ts ~f:(fun i ->
                    let new_repr = Caml.Format.asprintf "%a" Pprintast.core_type i.rtyp in
                    if String.equal new_repr typ_repr then raise (ItemFound i)
                  );
        None
    with ItemFound i -> Some i

    let map ~f (xs: t) = List.map ~f xs
    let empty = []
    let is_empty : t -> bool = List.is_empty
    let extend param_name rtyp ltyp ts =
(*      printf "extending by `%s`\n%!" param_name;*)
      {param_name; rtyp; ltyp} :: ts
end

(* TODO: maybe use Ppxlib.name_type_params_in_td ? *)
let extract_names = List.map ~f:(fun typ ->
    match typ.ptyp_desc with
    | Ptyp_var s -> s
    | _ -> failwith (Caml.Format.asprintf "Don't know what to do with %a" Pprintast.core_type typ)
  )

let nolabel = Asttypes.Nolabel

let get_param_names pcd_args =
  let Pcstr_tuple pcd_args = pcd_args in
  extract_names pcd_args

let mangle_construct_name name =
  let low = String.mapi ~f:(function 0 -> Char.lowercase | _ -> Fn.id) name in
  match low with
  | "val" | "if" | "else" | "for" | "do" | "let" | "open" | "not" -> low ^ "_"
  | _ -> low

let lower_lid lid = Location.{lid with txt = mangle_construct_name lid.Location.txt }

module Located = struct
  include Located
  (* let mknoloc txt = { txt; loc = Location.none } *)
  let map_loc ~f l = {l with txt = f l.txt}
end

module Exp = struct
  include Exp
  let mytuple ~loc ?(attrs=[]) = function
    | [] -> failwith "bad_argument: mytuple"
    | [x] -> x
    | xs -> tuple ~loc ~attrs:attrs xs
  let apply ~loc f = function
    | [] -> f
    | xs -> apply ~loc f xs
end

let prepare_distribs ~loc fully_abstract_tname tdecl fmap_decl =
  let open Longident in
  let constructors =
    match tdecl.ptype_kind with
    | Ptype_variant c -> c
    | _ -> failwith "not implemented"
  in
  let gen_module_str = Located.mk ~loc @@ Naming.functor_name fully_abstract_tname in

  let distrib_lid = Located.mk ~loc Longident.(Ldot (Lident gen_module_str.txt, "distrib")) in
  [ Str.module_ ~loc @@ Mb.mk ~loc (Located.mk ~loc "T")
    (Mod.structure ~loc
          [ fmap_decl
          ; Str.type_ ~loc Nonrecursive
              [ Type.mk ~loc
                  ~params:tdecl.ptype_params
                  ~kind:Ptype_abstract
                  ~priv: Public
                  ~cstrs:[]
                  ~manifest:(Typ.constr ~loc (Located.mk ~loc @@ lident tdecl.ptype_name.txt) @@
                              List.map ~f:fst tdecl.ptype_params)
                  (Located.mk ~loc "t") ]])
  ; Str.module_ ~loc @@ Mb.mk ~loc gen_module_str
      (Mod.apply ~loc
        (Mod.ident ~loc (Located.mk ~loc @@ Lident (match tdecl.ptype_params with [] -> "Fmap" | xs -> sprintf "Fmap%d" (List.length xs)) ))
        (Mod.ident ~loc (Located.mk ~loc @@ Lident "T")) )
  ] @ (List.map constructors ~f:(fun { pcd_name; pcd_args } ->
      let names = get_param_names pcd_args |> List.mapi ~f:(fun i _ -> sprintf "a%d" i) in
      let body =
        let constr_itself = function
          | [] -> Exp.construct (Located.mk ~loc @@ lident pcd_name.txt) None
          | args ->
              Exp.construct (Located.mk ~loc @@ lident pcd_name.txt) @@ Option.some @@
              (match args with [x] -> x | args -> Exp.mytuple ~loc args)
        in
        match names with
        | []    -> constr_itself []
        | [one] -> constr_itself [Exp.ident ~loc @@ Located.mk ~loc (Lident one)]
        | xs -> (* construct tuple here *)
            constr_itself (List.map ~f:(fun name -> Exp.ident ~loc @@ Located.mk ~loc (Lident name)) xs)
      in
      let body = [%expr inj [%e Exp.apply ~loc (Exp.ident ~loc distrib_lid) [nolabel, body] ] ] in
      Str.value ~loc Asttypes.Nonrecursive [
        Vb.mk ~loc (Pat.var ~loc @@ lower_lid pcd_name)
          (match names with
          | [] -> Exp.fun_ ~loc nolabel None (Pat.construct ~loc (Located.mk ~loc (Lident "()")) None) body
          | names -> List.fold_right ~f:(fun name acc -> Exp.fun_ ~loc nolabel None (Pat.var ~loc @@ Located.mk ~loc name) acc) names ~init:body)
      ]
    )
    )

(* At the moment we genrate fmap here but it is totally fine to reuse the one genrated by GT *)
let prepare_fmap ~loc tdecl =
  [%stri let rec fmap _eta = GT.gmap [%e Exp.ident ~loc (Located.mk ~loc @@ lident tdecl.ptype_name.txt) ] _eta]

let mangle_string s = s ^ "_ltyp"
let map_deepest_lident ~f lident =
  let rec helper = function
    | Lident s -> Lident (f s)
    | Ldot (l, s) -> Ldot (l, f s)
    | Lapply (l, r) -> Lapply (l, helper r)
  in
  helper lident

let mangle_lident lident = map_deepest_lident ~f:mangle_string lident

let mangle_core_type typ =
  let rec helper typ =
    let loc = typ.ptyp_loc in
    match typ with
    | [%type: _] -> assert false
    | [%type: string] -> [%type: string logic]
    | _ ->
        match typ.ptyp_desc with
        | Ptyp_var s -> typ
        | Ptyp_constr ({txt; loc}, params) ->
            Typ.constr ~loc {loc; txt = mangle_lident txt} @@
            List.map ~f:helper params
        | _ -> failwith "should not happen"
  in
  helper typ

let mangle_reifier typ =
  let rec helper typ =
    let loc = typ.ptyp_loc in
    match typ with
    | [%type: _] -> assert false
    | [%type: string]
    | [%type: int] -> [%expr OCanren.reify]
    | _ ->
        match typ.ptyp_desc with
        | Ptyp_var s -> Exp.ident ~loc @@ Located.lident ~loc ("f" ^ s)
        | Ptyp_constr ({txt; loc}, params) ->
          Exp.apply ~loc
            (Exp.ident ~loc @@ Located.mk ~loc (map_deepest_lident ~f:(fun s -> s^"_reify") txt))  @@
            List.map ~f:(fun typ -> Nolabel, helper typ) params
        | _ -> failwith "should not happen"
  in
  helper typ

let revisit_adt ~loc other_attrs tdecl ctors =
  let der_typ_name = tdecl.ptype_name.Asttypes.txt in

  (* Let's forget about mutal recursion for now *)
  (* For every constructor argument we need to put ground types to parameters *)
  let mapa, full_t =
    List.fold_right
      ~f:(fun cd (n, acc_map,cs) ->
          let n,map2,new_args = List.fold_right
            ~f:(fun typ (n,map,args) ->
                  match typ.ptyp_desc with
                  | Ptyp_any -> assert false
                  | Ptyp_var s -> (n, map, typ::args)
                  | Ptyp_constr ({txt;loc}, params) -> begin
                    match FoldInfo.param_for_rtyp typ map with
                    | Some {FoldInfo.param_name } ->
                      (n, map, (ptyp_var ~loc param_name)::args)
                    | None ->
                        (* We need to mangle whole constructor *)
                        let ltyp = mangle_core_type typ in
                        let new_name = sprintf "a%d" n in
                        (n+1, FoldInfo.extend new_name typ ltyp map,
                         (ptyp_var ~loc new_name)::args)
                  end
                  | _ ->
                    match FoldInfo.param_for_rtyp typ map with
                    | Some {FoldInfo.param_name } ->
                      (n, map, (ptyp_var ~loc param_name)::args)
                    | None ->
                      let new_name = sprintf "a%d" n in
                      (n+1, FoldInfo.extend new_name typ typ map,
                       (ptyp_var ~loc new_name)::args)
              )
            (match cd.pcd_args with Pcstr_tuple tt -> tt | Pcstr_record _ -> assert false)
            ~init:(n, acc_map,[])
          in
          let new_args = Pcstr_tuple new_args in
          (n, map2, { cd with pcd_args = new_args } :: cs)
      )
      ctors
      ~init:(0, FoldInfo.empty, [])
      |> (fun (_, mapa, cs) -> mapa,  { tdecl with  ptype_kind = Ptype_variant cs
                                                  ; ptype_attributes = other_attrs
                                      })
  in
  (* now we need to add some parameters if we collected ones *)
  let ans =
    if FoldInfo.is_empty mapa
    then
      let fmap_for_typ = prepare_fmap ~loc full_t in
      let ltyp =
        pstr_type ~loc Recursive
          [ { tdecl with
              ptype_kind = Ptype_abstract
            ; ptype_name = Located.mk ~loc (mangle_string der_typ_name)
            ; ptype_manifest = Some
                  (ptyp_constr ~loc (Located.lident ~loc "logic")
                     [ ptyp_constr ~loc (Located.lident ~loc der_typ_name) @@
                       List.map ~f:fst tdecl.ptype_params
                     ])
            ; ptype_attributes = other_attrs
            } ]
      in
      let ground_typ =
        pstr_type ~loc Nonrecursive [{full_t with ptype_attributes = other_attrs}]
      in
      let the_reifier =
        let reifiers = FoldInfo.map ~f:(fun {FoldInfo.rtyp} -> mangle_reifier rtyp) mapa in
        pstr_value ~loc Recursive
          [ value_binding ~loc ~pat:(ppat_var ~loc @@ Located.mk ~loc (der_typ_name ^ "_reify"))
              ~expr:[%expr fun h ->
                [%e
                   pexp_apply ~loc
                     (pexp_ident ~loc @@ Located.mk ~loc Longident.(Ldot (Lident (Naming.functor_name tdecl.ptype_name.txt), "reify")))
                     (List.map ~f:(fun t -> (Nolabel,t)) (reifiers @ [[%expr h]]))
                ]
              ]
          ]
      in
      ground_typ :: ltyp :: (prepare_distribs der_typ_name ~loc full_t fmap_for_typ) @ [the_reifier]
    else
      let functorized_type = Naming.fabst_name full_t.ptype_name.txt in
      let fully_abstract_typ =
        (* a type name for which we will generate `fmap` *)
        let extra_params = FoldInfo.map mapa
          ~f:(fun fi -> (Ast_helper.Typ.var fi.FoldInfo.param_name, Asttypes.Invariant))
        in
        let open Location in
        {full_t with ptype_params = full_t.ptype_params @ extra_params;
                     ptype_name = { full_t.ptype_name with txt = functorized_type }}
      in
      let fully_abstract_tdecl = pstr_type ~loc Nonrecursive [fully_abstract_typ] in

      let ground_typ =
        let alias =
          let old_params = List.map ~f:fst tdecl.ptype_params  in
          let extra_params = FoldInfo.map ~f:(fun {FoldInfo.rtyp} -> rtyp)  mapa in
          Typ.constr ~loc
            (Located.lident ~loc fully_abstract_typ.ptype_name.Asttypes.txt)
            (old_params @ extra_params)
        in
        pstr_type ~loc Recursive
          [{ tdecl with ptype_manifest = (Some alias)
                      ; ptype_kind=Ptype_abstract
                      ; ptype_attributes = other_attrs
            }]
      in
      let logic_typ =
        let alias =
          let old_params = List.map ~f:fst tdecl.ptype_params  in
          let extra_params = FoldInfo.map ~f:(fun {FoldInfo.ltyp} -> ltyp) mapa in
          Typ.constr ~loc (Located.lident ~loc "logic")
            [ Typ.constr ~loc (Located.lident ~loc fully_abstract_typ.ptype_name.Asttypes.txt)
                (old_params @ extra_params)
            ]
        in
        pstr_type ~loc Recursive
          [ { tdecl with
              ptype_kind = Ptype_abstract
            ; ptype_manifest = Some alias
            ; ptype_name = Located.map mangle_string tdecl.ptype_name
            ; ptype_attributes = other_attrs
            } ]
      in

      let fmap_for_typ = prepare_fmap ~loc fully_abstract_typ in


      let distribs = prepare_distribs ~loc der_typ_name fully_abstract_typ fmap_for_typ in
      let the_reifier =
        let reifiers = FoldInfo.map ~f:(fun {FoldInfo.rtyp} -> mangle_reifier rtyp) mapa in
        pstr_value ~loc Recursive
          [ value_binding ~loc ~pat:(ppat_var ~loc @@ Located.mk ~loc (der_typ_name ^ "_reify"))
              ~expr:[%expr fun eta ->
                [%e
                   pexp_apply ~loc
                     (pexp_ident ~loc @@
                      Located.mk ~loc Longident.(Ldot (Lident (Naming.functor_name tdecl.ptype_name.txt), "reify")))
                     (List.map ~f:(fun t -> (Nolabel,t)) (reifiers @ [[%expr eta]]))
                ]
              ]
          ]
      in

      fully_abstract_tdecl :: ground_typ :: logic_typ :: distribs @ [the_reifier]
  in
  ans

let has_to_gen_attr (xs: attributes) =
  let ours,others = List.partition_map xs ~f:(fun ({attr_name={txt};_} as attr) ->
      if String.equal txt "distrib" then `Fst attr else `Snd attr
      )
  in
  assert (List.length ours <= 1);
  match ours with
  | [] -> None
  | [h] -> Some (h, others)
  | _ -> failwith "to many distrib attributes"

let suitable_tydecl_wrap ~on_ok ~on_fail tdecl =
  match tdecl.ptype_kind with
  | Ptype_variant cs when Option.is_none tdecl.ptype_manifest -> begin
      match has_to_gen_attr tdecl.ptype_attributes with
      | None -> on_fail ()
      | Some (our, other_attrs) ->
          Attribute.explicitly_drop#type_declaration tdecl;
          on_ok cs other_attrs { tdecl with ptype_attributes = [] }
    end
  | _ -> on_fail ()

let suitable_tydecl =
  suitable_tydecl_wrap ~on_ok:(fun _ _ _ -> true) ~on_fail:(fun () -> false)

let str_type_decl ~loc (flg,tdls) =
  let wrap_tydecls loc ts =
    let f tdecl =
      suitable_tydecl_wrap tdecl
        ~on_ok:(fun cs other_attrs tdecl -> revisit_adt ~loc other_attrs tdecl cs)
        ~on_fail:(fun () ->
            failwith "Only variant types without manifest are supported")
    in
    List.concat (List.map ~f ts)
  in
  wrap_tydecls loc tdls
