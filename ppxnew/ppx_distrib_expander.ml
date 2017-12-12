open Ppx_core
open Ast_builder.Default
open Printf
let (@@) = Caml.(@@)

(*
let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

(* Simplifies match cases, for readability of the generated code. It's not obvious we can
   stick this in ppx_core, as (match e1 with p -> e2) and (let p = e1 in e2) are not typed
   exactly the same (type inference goes in different order, meaning type disambiguation
   differs). *)
let pexp_match ~loc expr cases =
  match cases with
  | [{ pc_lhs; pc_guard = None; pc_rhs }] ->
    begin match pc_lhs, expr with
    | { ppat_attributes = []; ppat_desc = Ppat_var { txt = ident; _ }; _ },
      { pexp_attributes = []; pexp_desc = Pexp_ident { txt = Lident ident'; _ }; _ }
      when String.equal ident ident' -> pc_rhs
    | _ ->
      pexp_let ~loc Nonrecursive
        [value_binding ~loc ~pat:pc_lhs ~expr]
        pc_rhs
    end
  | _ -> pexp_match ~loc expr cases
*)
module Attrs = struct
  let default =
    Attribute.declare "sexp.default"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)

  let drop_default =
    Attribute.declare "sexp.sexp_drop_default"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()

  let drop_if =
    Attribute.declare "sexp.sexp_drop_if"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)
end
(*
module Fun_or_match = struct
  type t =
    | Fun   of expression
    | Match of case list

  let expr ~loc t =
    match t with
    | Fun f       -> f
    | Match cases -> pexp_function ~loc cases

  let unroll ~loc e t =
    match t with
    | Fun f       -> eapply ~loc f [e]
    | Match cases -> pexp_match ~loc e cases

  let map_tmp_vars ~loc ts =
    let vars = List.mapi ts ~f:(fun i _ -> "v" ^ Int.to_string i) in
    let bindings =
      List.map2_exn vars ts ~f:(fun var t ->
        let expr = unroll ~loc (evar ~loc var) t in
        value_binding ~loc ~pat:(pvar ~loc var) ~expr)
    in
    (bindings,
     List.map vars ~f:(pvar ~loc),
     List.map vars ~f:(evar ~loc))
end

(* A renaming is a mapping from type variable name to type variable name.
   In definitions such as:

   type 'a t =
   | A : <type> -> 'b t
   | B of 'a

   we generate a function that takes an sexp_of parameter named after 'a, but 'a is not in
   scope in <type> when handling the constructor A (because A is a gadt constructor).
   Instead the type variables in scope are the ones defined in the return type of A,
   namely 'b. There could be less or more type variable in cases such as:

   type _ less = Less : int less
   type _ more = More : ('a * 'a) more

   If for instance, <type> is ['b * 'c], when we find 'b, we will look for ['b] in the
   renaming and find ['a] (only in that gadt branch, it could be something else in other
   branches), at which point we can call the previously bound sexp_of parameter named
   after 'a.
   If we can't find a resulting name, like when looking up ['c] in the renaming, then we
   assume the variable is existentially quantified and treat it as [_] (which is ok,
   assuming there are no constraints). *)
module Renaming : sig
  type t
  val identity : t
  val add_universally_bound : t -> string -> t

  type binding_kind =
    | Universally_bound of string
    | Existentially_bound

  val binding_kind : t -> string -> binding_kind

  val of_gadt : string list -> constructor_declaration -> t
end = struct
  type error = string Loc.t
  type t = (string, error) Result.t Map.M(String).t option

  let identity = None

  type binding_kind =
    | Universally_bound of string
    | Existentially_bound

  let add_universally_bound (t : t) name : t =
    match t with
    | None -> None
    | Some map -> Some (Map.add ~key:name ~data:(Ok name) map)

  let binding_kind t var =
    match t with
    | None -> Universally_bound var
    | Some map ->
      match Map.find map var with
      | None                      -> Existentially_bound
      | Some (Ok value)           -> Universally_bound value
      | Some (Error { loc; txt }) -> raise (Location.raise_errorf ~loc "%s" txt)

  (* Return a map translating type variables appearing in the return type of a GADT
     constructor to their name in the type parameter list.

     For instance:

     {[
       type ('a, 'b) t = X : 'x * 'y -> ('x, 'y) t
     ]}

     will produce:

     {[
       "x" -> Ok "a"
       "y" -> Ok "b"
     ]}

     If a variable appears twice in the return type it will map to [Error _]. If a
     variable cannot be mapped to a parameter of the type declaration, it will map to
     [Error] (for instance [A : 'a -> 'a list t]).

     It returns None on user error, to let the typer give the error message *)
  let of_gadt =
    (* Add all type variables of a type to a map. *)
    let add_typevars = object
      inherit [ (string, error) Result.t Map.M(String).t ] Ast_traverse.fold
        as super
      method! core_type ty map =
        match ty.ptyp_desc with
        | Ptyp_var var ->
          let error =
            { loc = ty.ptyp_loc
            ; txt = "ppx_sexp_conv: variable is not a parameter of the type constructor"
            }
          in
          Map.add map ~key:var ~data:(Error error)
        | _ -> super#core_type ty map
    end in

    let aux map tp_name tp_in_return_type =
      match tp_in_return_type.ptyp_desc with
      | Ptyp_var var ->
        let data =
          if Map.mem map var then
            let loc = tp_in_return_type.ptyp_loc in
            Error { loc; txt = "ppx_sexp_conv: duplicate variable" }
          else
            Ok tp_name
        in
        Map.add map ~key:var ~data
      | _ ->
        add_typevars#core_type tp_in_return_type map
    in

    fun tps cd ->
      match cd.pcd_res with
      | None -> None
      | Some ty ->
        match ty.ptyp_desc with
        | Ptyp_constr (_, params) ->
          if List.length params <> List.length tps then
            None
          else
            Some (Caml.ListLabels.fold_left2 tps params ~init:(Map.empty (module String))
                    ~f:aux)
        | _ ->
          None
end

(* Utility functions *)

let replace_variables_by_underscores =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_var _ -> Ptyp_any
      | t -> super#core_type_desc t
  end in
  map#core_type

let rigid_type_var ~type_name x =
  let prefix = "rigid_" in
  if String.equal x type_name || String.is_prefix x ~prefix
  then prefix ^ x ^ "_of_type_" ^ type_name
  else x

let make_type_rigid ~type_name =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type ty =
      let ptyp_desc =
        match ty.ptyp_desc with
        | Ptyp_var s ->
          Ptyp_constr (Located.lident ~loc:ty.ptyp_loc (rigid_type_var ~type_name s), [])
        | desc -> super#core_type_desc desc
      in
      { ty with ptyp_desc }
  end in
  map#core_type

(* Generates the quantified type [ ! 'a .. 'z . (make_mono_type t ('a .. 'z)) ] or
   [type a .. z. make_mono_type t (a .. z)] when [use_rigid_variables] is true.
   Annotation are needed for non regular recursive datatypes and gadt when the return type
   of constructors are constrained. Unfortunately, putting rigid variables everywhere does
   not work because of certains types with constraints. We thus only use rigid variables
   for sum types, which includes all GADTs. *)

let tvars_of_core_type : (core_type -> string list) =
  let tvars = object
    inherit [string list] Ast_traverse.fold as super
    method! core_type x acc =
      match x.ptyp_desc with
      | Ptyp_var x -> if List.mem acc x ~equal:String.equal then acc else x :: acc
      | _ ->
        super#core_type x acc
  end
  in fun typ ->
    List.rev (tvars#core_type typ [])

let constrained_function_binding = fun
  (* placing a suitably polymorphic or rigid type constraint on the pattern or body *)
  (loc:Location.t) (td:type_declaration) (typ:core_type) ~(tps:string list)
  ~(func_name:string) (body:expression)
->
  let vars = tvars_of_core_type typ in
  let has_vars = match vars with [] -> false | _::_ -> true in
  let pat =
    let pat = pvar ~loc func_name in
    if not has_vars then pat else
      ppat_constraint ~loc pat (ptyp_poly ~loc vars typ)
  in
  let body =
    let use_rigid_variables =
      match td.ptype_kind with | Ptype_variant _ -> true | _ -> false
    in
    if use_rigid_variables
    then
      let type_name = td.ptype_name.txt in
      List.fold_right tps
        ~f:(fun tp body -> pexp_newtype ~loc (rigid_type_var ~type_name tp) body)
        ~init:(pexp_constraint ~loc body (make_type_rigid ~type_name typ))
    else
      if has_vars
      then body
      else pexp_constraint ~loc body typ
  in
  value_binding ~loc ~pat ~expr:body

let sexp_type_is_recursive =
  types_are_recursive ~short_circuit:(fun typ ->
    match typ with
    | [%type: [%t? _] sexp_opaque ] -> Some false
    | _ -> None)

let really_recursive rec_flag tds =
  match rec_flag with
  | Recursive    -> if sexp_type_is_recursive tds then Recursive else Nonrecursive
  | Nonrecursive -> Nonrecursive

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct
  let type_of_sexp_of ~loc t =
    [%type: [%t t] -> Sexplib.Sexp.t]

  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_sexp_of

  let mk_sig ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      psig_value ~loc
        (value_description ~loc
           ~name:(Located.map ((^) "sexp_of_") td.ptype_name)
           ~type_:(mk_type td)
           ~prim:[]))

  let mk_sig_exn ~loc:_ ~path:_ _te = []
end

let is_polymorphic_variant =
  let rec check = function
    | { ptyp_desc = Ptyp_variant _; _ } -> `Definitely
    | { ptyp_desc = Ptyp_alias (typ,_); _ } -> check typ
    | { ptyp_desc = Ptyp_constr _; _ } -> `Maybe
    | _ -> `Surely_not (* Type vars go here even though they could be polymorphic
                          variants, however we don't handle it if they get substituted
                          by a polymorphic variant that is then included. *)
  in
  fun td ~sig_ ->
    match td.ptype_kind with
    | Ptype_variant _ | Ptype_record _ | Ptype_open -> `Surely_not
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> if sig_ then `Maybe else `Surely_not
      | Some typ -> check typ

(* Generates the signature for type conversion from S-expressions *)
module Sig_generate_of_sexp = struct
  let type_of_of_sexp ~loc t =
    [%type: Sexplib.Sexp.t -> [%t t]]

  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_of_sexp

  let sig_of_td with_poly td =
    let of_sexp_type = mk_type td in
    let loc = td.ptype_loc in
    let of_sexp_item =
      psig_value ~loc
        (value_description ~loc
           ~name:(Located.map (fun s -> s ^ "_of_sexp") td.ptype_name)
           ~type_:of_sexp_type
           ~prim:[])
    in
    match with_poly, is_polymorphic_variant td ~sig_:true with
    | true, `Surely_not ->
      Location.raise_errorf ~loc
        "Sig_generate_of_sexp.sig_of_td: sexp_poly annotation \
         but type is surely not a polymorphic variant"
    | false, (`Surely_not | `Maybe) -> [of_sexp_item]
    | (true | false), `Definitely
    | true, `Maybe ->
      [ of_sexp_item
      ; psig_value ~loc
          (value_description ~loc
             ~name:(Located.map (fun s -> "__" ^ s ^ "_of_sexp__") td.ptype_name)
             ~type_:of_sexp_type
             ~prim:[])
      ]

  let mk_sig ~poly ~loc:_ ~path:_ (_rf, tds) =
    List.concat_map tds ~f:(sig_of_td poly)
end

module Str_generate_sexp_of = struct
  (* Handling of record defaults *)

  type record_field_handler = [ `keep | `drop_default | `drop_if of expression ]

  let get_record_field_handler ~loc ld : record_field_handler =
    match
      Attribute.get Attrs.drop_default ld,
      Attribute.get Attrs.drop_if ld
    with
    | None, None -> `keep
    | Some (), None -> `drop_default
    | None, Some e -> `drop_if e
    | Some (), Some _ ->
      Location.raise_errorf ~loc "sexp record field handler defined twice"

  let sexp_of_type_constr ~loc id args =
    type_constr_conv ~loc id ~f:(fun s -> "sexp_of_" ^ s) args

  (* Conversion of types *)
  let rec sexp_of_type (renaming : Renaming.t) typ : Fun_or_match.t =
    let loc = typ.ptyp_loc in
    match typ with
    | [%type:  _ ] ->
      Fun [%expr  fun _ -> Sexplib.Sexp.Atom "_" ]
    | [%type: [%t? _] sexp_opaque ] ->
      Fun [%expr  Sexplib.Conv.sexp_of_opaque ]
    | { ptyp_desc = Ptyp_tuple tp; _ } -> Match [sexp_of_tuple renaming (loc,tp)]
    | { ptyp_desc = Ptyp_var parm; _ } ->
      begin match Renaming.binding_kind renaming parm with
      | Universally_bound parm -> Fun (evar ~loc ("_of_" ^ parm))
      | Existentially_bound -> sexp_of_type renaming [%type:  _ ]
      end
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      Fun (sexp_of_type_constr ~loc id
             (List.map args
                ~f:(fun tp -> Fun_or_match.expr ~loc (sexp_of_type renaming tp))))
    | { ptyp_desc = Ptyp_arrow (_,_,_); _ } ->
      Fun [%expr  fun _f -> Sexplib.Conv.(sexp_of_fun ignore) ]
    | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      sexp_of_variant renaming (loc,row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      sexp_of_poly renaming parms poly_tp
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ }
      ->
      Location.raise_errorf ~loc "Type unsupported for ppx [sexp_of] conversion"

  (* Conversion of tuples *)
  and sexp_of_tuple renaming (loc,tps) =
    let fps = List.map ~f:(fun tp -> sexp_of_type renaming tp) tps in
    let bindings, pvars, evars = Fun_or_match.map_tmp_vars ~loc fps in
    let in_expr = [%expr  Sexplib.Sexp.List [%e elist ~loc evars] ] in
    let expr = pexp_let ~loc Nonrecursive bindings in_expr in
    ppat_tuple ~loc pvars --> expr

  (* Conversion of variant types *)

  and sexp_of_variant renaming ((loc,row_fields):(Location.t * row_field list))
    : Fun_or_match.t =
    let item = function
      | Rtag (cnstr,_,true,[]) ->
        ppat_variant ~loc cnstr None -->
        [%expr Sexplib.Sexp.Atom [%e estring ~loc cnstr]]
      | Rtag (cnstr,_,_,[ [%type: [%t? tp] sexp_list] ]) ->
        let cnv_expr = Fun_or_match.expr ~loc (sexp_of_type renaming tp) in
        ppat_variant ~loc cnstr (Some [%pat? l]) -->
        [%expr
          Sexplib.Sexp.List
            ( Sexplib.Sexp.Atom [%e estring ~loc cnstr] ::
              Sexplib.Conv.list_map [%e cnv_expr] l
            )
        ]
      | Rtag (cnstr,_,false,[tp]) ->
        let cnstr_expr = [%expr Sexplib.Sexp.Atom [%e estring ~loc cnstr] ] in
        let var, patt = evar ~loc "v0", pvar ~loc "v0" in
        let cnstr_arg = Fun_or_match.unroll ~loc var (sexp_of_type renaming tp) in
        let expr = [%expr Sexplib.Sexp.List [%e elist ~loc [cnstr_expr; cnstr_arg]]] in
        ppat_variant ~loc cnstr (Some patt) --> expr

      | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
        ppat_alias ~loc (ppat_type ~loc id) (Loc.make "v" ~loc) -->
        sexp_of_type_constr ~loc id [[%expr v]]
      | Rtag (_,_,true,[_])
      | Rtag (_,_,_,_::_::_) ->
        Location.raise_errorf ~loc "unsupported: sexp_of_variant/Rtag/&"

      | Rinherit ({ ptyp_desc = Ptyp_constr (id, _::_); _ } as typ) ->
        let call = Fun_or_match.expr ~loc (sexp_of_type renaming typ) in
        ppat_alias ~loc (ppat_type ~loc id) (Loc.make "v" ~loc) -->
        [%expr [%e call] v]

      | Rinherit _ ->
        Location.raise_errorf ~loc
          "unsupported: sexp_of_variant/Rinherit/non-id" (* impossible?*)

      | Rtag (_,_,false,[]) ->
        assert false
    in
    Match (List.map ~f:item row_fields)

  (* Polymorphic record fields *)

  and sexp_of_poly renaming parms tp =
    let loc = tp.ptyp_loc in
    let bindings =
      let mk_binding parm =
        value_binding ~loc ~pat:(pvar ~loc ("_of_" ^ parm))
          ~expr:[%expr Sexplib.Conv.sexp_of_opaque]
      in
      List.map ~f:mk_binding parms
    in
    let renaming = List.fold_left parms ~init:renaming ~f:Renaming.add_universally_bound in
    match sexp_of_type renaming tp with
    | Fun fun_expr -> Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | Match matchings ->
      Match
        [ [%pat? arg] -->
          pexp_let ~loc Nonrecursive bindings
            (pexp_match ~loc [%expr arg] matchings)
        ]

  (* Conversion of record types *)

  let mk_rec_patt loc patt name =
    let p =
      Loc.make (Longident.Lident name) ~loc ,
      pvar ~loc ("v_" ^ name)
    in
    patt @ [p]

  let sexp_of_record_field ~renaming patt expr name tp ?sexp_of is_empty_expr =
    let loc = tp.ptyp_loc in
    let patt = mk_rec_patt loc patt name in
    let cnv_expr = match (sexp_of_type renaming tp) with
      | Fun exp -> exp
      | Match matchings -> [%expr fun el -> [%e pexp_match ~loc [%expr el] matchings]]
    in
    let cnv_expr =
      match sexp_of with
      | None -> cnv_expr
      | Some sexp_of -> [%expr  [%e sexp_of] [%e cnv_expr] ]
    in
    let expr =
      let v_name = [%expr  [%e  "v_" ^ name] ] in
      [%expr
        let bnds =
          if [%e is_empty_expr loc (evar ~loc v_name)] then bnds
          else
            let arg = [%e cnv_expr] [%e evar ~loc v_name] in
            let bnd =
              Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]; arg]
            in
            bnd :: bnds
        in
        [%e expr]
        ]
    in
    patt, expr

  let sexp_of_default_field ~renaming patt expr name tp ?sexp_of default =
    sexp_of_record_field ~renaming patt expr name tp ?sexp_of
      (fun loc expr -> [%expr  Sexplib.Conv.(=) [%e default] [%e expr] ])

  let sexp_of_label_declaration_list ~renaming loc flds ~wrap_expr =
    let list_empty_expr loc lst =
      [%expr
          match [%e lst] with
          | [] -> true
          | _ -> false ]
    in
    let array_empty_expr loc arr =
      [%expr
          match [%e arr] with
          | [||] -> true
          | _ -> false ]
    in
    let coll ((patt : (Longident.t loc * pattern) list), expr) = function
      | {pld_name = {txt=name; loc};
         pld_type = [%type: [%t? tp] sexp_option]; _ } ->
        let patt = mk_rec_patt loc patt name in
        let vname = [%expr  v ] in
        let cnv_expr = Fun_or_match.unroll ~loc vname (sexp_of_type renaming tp) in
        let expr =
          [%expr
           let bnds =
             match [%e evar ~loc ("v_" ^ name)] with
             | None -> bnds
             | Some v ->
               let arg = [%e cnv_expr] in
               let bnd =
                 Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]; arg]
               in
               bnd :: bnds
           in
           [%e expr]
          ]
        in
        patt, expr
      | {pld_name = {txt=name; loc};
         pld_type = [%type: sexp_bool]; _ } ->
        let patt = mk_rec_patt loc patt name in
        let expr =
          [%expr
           let bnds =
             if [%e evar ~loc ("v_" ^ name)] then
               let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]] in
               bnd :: bnds
             else bnds
           in
           [%e expr]
          ]
        in
        patt, expr
      | {pld_name = {txt=name; loc};
         pld_type = [%type: [%t? tp] sexp_list]; _ } ->
        sexp_of_record_field ~renaming patt expr name tp
          ~sexp_of:[%expr  sexp_of_list ] list_empty_expr
      | {pld_name = {txt=name; loc};
         pld_type = [%type: [%t? tp] sexp_array]; _ } ->
        sexp_of_record_field ~renaming patt expr name tp
          ~sexp_of:[%expr  sexp_of_array ] array_empty_expr
      | {pld_name = {txt=name; loc}; pld_type = tp; _ } as ld ->
        begin match get_record_field_handler ~loc ld with
        | `drop_default -> begin
            match Attribute.get Attrs.default ld with
            | None ->
              Location.raise_errorf ~loc "no default to drop"
            | Some default ->
              sexp_of_default_field ~renaming patt expr name tp default
          end
        | `drop_if test ->
          sexp_of_record_field ~renaming patt expr name tp
            (fun loc expr -> [%expr [%e test] [%e expr]])
        | `keep ->
          let patt = mk_rec_patt loc patt name in
          let vname = evar ~loc ("v_" ^ name) in
          let cnv_expr = Fun_or_match.unroll ~loc vname (sexp_of_type renaming tp) in
          let expr =
            [%expr
             let arg = [%e cnv_expr] in
             let bnd =
               Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]; arg]
             in
             let bnds = bnd :: bnds in
             [%e expr]
            ]
          in
          patt, expr
        end
    in
    let init_expr = wrap_expr [%expr bnds] in
    let patt, expr =
      List.fold_left ~f:coll ~init:([], init_expr) flds
    in
    ppat_record ~loc patt Closed, [%expr let bnds = [] in [%e expr]]

  (* Conversion of sum types *)

  let branch_sum renaming ~loc constr_lid constr_str args =
    match args with
    | Pcstr_record lds ->
      let cnstr_expr = [%expr Sexplib.Sexp.Atom [%e constr_str] ] in
      let patt, expr =
        (* Uncomment to wrap record *)
        (* sexp_of_label_declaration_list loc lds
         *   ~wrap_expr:(fun expr ->
         *     [%expr Sexplib.Sexp.List [ [%e cnstr_expr];
         *                                Sexplib.Sexp.List [%e expr]
         *                              ]
         *     ]) *)
        sexp_of_label_declaration_list ~renaming loc lds
          ~wrap_expr:(fun expr ->
            [%expr Sexplib.Sexp.List ([%e cnstr_expr] :: [%e expr])])
      in
      ppat_construct ~loc constr_lid (Some patt) --> expr
    | Pcstr_tuple pcd_args ->
      match pcd_args with
      | [] ->
        ppat_construct ~loc constr_lid None --> [%expr Sexplib.Sexp.Atom [%e constr_str]]
      | args ->
        match args with
        | [ [%type: [%t? tp] sexp_list ] ] ->
          let cnv_expr = Fun_or_match.expr ~loc (sexp_of_type renaming tp) in
          ppat_construct ~loc constr_lid (Some [%pat? l]) -->
          [%expr
            Sexplib.Sexp.List
              (Sexplib.Sexp.Atom [%e constr_str] ::
               Sexplib.Conv.list_map [%e cnv_expr] l)]
        | _ ->
          let sexp_of_args = List.map ~f:(sexp_of_type renaming) args in
          let cnstr_expr = [%expr Sexplib.Sexp.Atom [%e constr_str] ] in
          let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc sexp_of_args in
          let patt =
            match patts with
            | [patt] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          ppat_construct ~loc constr_lid (Some patt) -->
          pexp_let ~loc Nonrecursive bindings
            [%expr Sexplib.Sexp.List [%e elist ~loc (cnstr_expr :: vars)]]

  let sexp_of_sum tps cds =
    Fun_or_match.Match (
      List.map cds ~f:(fun cd ->
        let renaming = Renaming.of_gadt tps cd in
        let constr_lid = Located.map lident cd.pcd_name in
        let constr_str = estring ~loc:cd.pcd_name.loc cd.pcd_name.txt in
        branch_sum renaming ~loc:cd.pcd_loc constr_lid constr_str cd.pcd_args
      )
    )

  (* Empty type *)
  let sexp_of_nil loc = Fun_or_match.Fun [%expr  fun _v -> assert false ]

  (* Generate code from type definitions *)

  let sexp_of_td td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:(fun tp -> (get_type_param_name tp).txt) in
    let {ptype_name = {txt = type_name; loc = _}; ptype_loc = loc; _} = td in
    let body =
      let body =
        match td.ptype_kind with
        | Ptype_variant cds -> sexp_of_sum tps cds
        | Ptype_record  lds ->
          let renaming = Renaming.identity in
          let patt, expr =
            sexp_of_label_declaration_list ~renaming loc lds
              ~wrap_expr:(fun expr -> [%expr Sexplib.Sexp.List [%e expr]]) in
          Match [patt --> expr]
        | Ptype_open -> Location.raise_errorf ~loc
                          "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          match td.ptype_manifest with
          | None    -> sexp_of_nil loc
          | Some { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
            sexp_of_variant Renaming.identity (loc,row_fields)
          | Some ty -> sexp_of_type Renaming.identity ty
      in
      let is_private_alias =
        match td.ptype_kind, td.ptype_manifest, td.ptype_private with
        | Ptype_abstract, Some _, Private -> true
        | _ -> false
      in
      if is_private_alias then
        (* Replace all type variable by _ to avoid generalization problems *)
        let ty_src =
          core_type_of_type_declaration td
          |> replace_variables_by_underscores
        in
        let manifest =
          match td.ptype_manifest with
          | Some manifest -> manifest
          | None -> Location.raise_errorf ~loc "sexp_of_td/no-manifest"
        in
        let ty_dst = replace_variables_by_underscores manifest in
        let coercion = [%expr  (v : [%t ty_src] :> [%t ty_dst]) ] in
        match body with
        | Fun fun_expr ->
          [%expr  fun v -> [%e fun_expr] [%e coercion] ]
        | Match matchings ->
          [%expr  fun v -> [%e pexp_match ~loc coercion matchings]]
      else
        match body with
          (* Prevent violation of value restriction and problems with recursive types by
             eta-expanding function definitions *)
        | Fun fun_expr -> [%expr fun v -> [%e fun_expr] v ]
        | Match matchings -> pexp_function ~loc matchings
    in
    let typ = Sig_generate_sexp_of.mk_type td in
    let func_name = "sexp_of_" ^ type_name in
    let body =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_of_" ^ id)) in
      eabstract ~loc patts body
    in
    [constrained_function_binding loc td typ ~tps ~func_name body]

  let sexp_of_tds ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let bindings = List.map tds ~f:sexp_of_td |> List.concat in
    pstr_value_list ~loc rec_flag bindings

  let sexp_of_exn ~loc:_ ~path ec =
    let renaming = Renaming.identity in
    let get_full_cnstr str = path ^ "." ^ str in
    let loc = ec.pext_name.loc in
    let expr =
      match ec with
      | {pext_name = cnstr;
         pext_kind = Pext_decl (extension_constructor_kind, None); _;} ->
        let constr_lid = Located.map lident cnstr in
        let converter = branch_sum renaming ~loc
                          constr_lid
                          (estring ~loc (get_full_cnstr cnstr.txt))
                          extension_constructor_kind in
        let assert_false = ppat_any ~loc --> [%expr assert false] in
        [%expr
          Sexplib.Conv.Exn_converter.add
            [%extension_constructor [%e pexp_construct ~loc constr_lid None]]
            [%e Fun_or_match.expr ~loc (Match [converter; assert_false])]
        ]
      | { pext_kind = Pext_decl (_, Some _); _} ->
        Location.raise_errorf ~loc "sexp_of_exn/:"
      | { pext_kind = Pext_rebind _; _} ->
        Location.raise_errorf ~loc "sexp_of_exn/rebind"
    in
    [ pstr_value ~loc Nonrecursive [value_binding ~loc ~pat:[%pat? ()] ~expr] ]
end

module Str_generate_of_sexp = struct

  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    [[%pat? Sexplib.Conv_error.No_variant_match] --> expr]

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc ~match_last matches =
    match match_last, matches with
    | true, [{pc_lhs = _; pc_guard = None; pc_rhs = expr}]
    | _, [{pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = expr}]
      -> expr
    | _ ->
      pexp_match ~loc [%expr atom] matches

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches loc rev_els call =
    let coll_structs acc (loc, cnstr) =
      pstring ~loc cnstr -->
      (match call with
       | `ptag_no_args -> [%expr Sexplib.Conv_error.ptag_no_args _tp_loc _sexp]
       | `ptag_takes_args -> [%expr Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp])
      :: acc
    in
    let exc_no_variant_match =
      [%pat? _] --> [%expr Sexplib.Conv_error.no_variant_match ()]
    in
    List.fold_left ~f:coll_structs ~init:[exc_no_variant_match] rev_els

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let split_row_field ~loc (atoms, structs, ainhs, sinhs) row_field =
    match row_field with
    | Rtag (cnstr,_,true,[]) ->
      let tpl = loc, cnstr in
      (
        tpl :: atoms,
        structs,
        `A tpl :: ainhs,
        sinhs
      )
    | Rtag (cnstr,_,false,[tp]) ->
      let loc = tp.ptyp_loc in
      (
        atoms,
        (loc, cnstr) :: structs,
        ainhs,
        `S (loc, cnstr, tp) :: sinhs
      )
      | Rinherit inh ->
        let iinh = `I inh in
        (
          atoms,
          structs,
          iinh :: ainhs,
          iinh :: sinhs
        )
      | Rtag (_,_,true,[_])
      | Rtag (_,_,_,_::_::_) ->
        Location.raise_errorf ~loc "split_row_field/&"
      | Rtag (_,_,false,[]) ->
        assert false

  let type_constr_of_sexp ?(internal=false) id args =
    type_constr_conv id args ~f:(fun s ->
      let s = s ^ "_of_sexp" in
      if internal then "__" ^ s ^ "__" else s
    )

  (* Conversion of types *)
  let rec type_of_sexp ?(internal=false) typ : Fun_or_match.t =
    let loc = typ.ptyp_loc in
    match typ with
    | [%type: [%t? _] sexp_opaque ]
    | [%type: _ ] ->
      Fun [%expr  Sexplib.Conv.opaque_of_sexp ]
    (*| [%type: sexp_option ] -> (* will never match surely! *)
      Fun [%expr  fun a_of_sexp v -> Some (a_of_sexp v) ]*)
    | [%type: [%t? ty1] sexp_list ] ->
      let arg1 = Fun_or_match.expr ~loc (type_of_sexp ty1) in
      Fun [%expr (fun a_of_sexp v -> Sexplib.Conv.list_of_sexp  a_of_sexp v) [%e arg1]]
    | [%type: [%t? ty1] sexp_array ] ->
      let arg1 = Fun_or_match.expr ~loc (type_of_sexp ty1) in
      Fun [%expr (fun a_of_sexp v -> Sexplib.Conv.array_of_sexp a_of_sexp v) [%e arg1] ]
    | { ptyp_desc = Ptyp_tuple tp; _ } -> Match (tuple_of_sexp (loc,tp))
    | { ptyp_desc = Ptyp_var parm; _ } -> Fun (evar ~loc ("_of_" ^ parm))
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      let args = List.map args ~f:(fun arg -> Fun_or_match.expr ~loc (type_of_sexp arg)) in
      Fun (type_constr_of_sexp ~loc ~internal id args)

    | { ptyp_desc = Ptyp_arrow (_,_,_); _ } -> Fun [%expr  Sexplib.Conv.fun_of_sexp ]
    | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
        variant_of_sexp ?full_type:None (loc,row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } -> poly_of_sexp parms poly_tp
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ }
      -> Location.raise_errorf ~loc "Type unsupported for ppx [of_sexp] conversion"

  (* Conversion of tuples *)
  and tuple_of_sexp (loc,tps) =
    let fps = List.map ~f:type_of_sexp tps in
    let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
    let n = List.length fps in
    [ [%pat? Sexplib.Sexp.List [%p plist ~loc patts]] -->
      pexp_let ~loc Nonrecursive bindings
        (pexp_tuple ~loc vars)
    ; [%pat? sexp] -->
      [%expr Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc
               [%e eint ~loc n]
               sexp]
    ]

  (* Generate code for matching included variant types *)
  and handle_variant_inh full_type ~match_last other_matches inh =
    let loc = inh.ptyp_loc in
    let func_expr = type_of_sexp ~internal:true inh in
    let app : Fun_or_match.t =
      let fun_expr = Fun_or_match.expr ~loc func_expr in
      Fun [%expr [%e fun_expr] _sexp]
    in
    let match_exc =
      handle_no_variant_match loc (
        handle_variant_match_last loc ~match_last other_matches) in
    let new_other_matches =
      [ [%pat? _] -->
        pexp_try ~loc
          [%expr ([%e Fun_or_match.expr ~loc app]
                  :> [%t replace_variables_by_underscores full_type])]
          match_exc
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs =
    let coll (other_matches, match_last) = function
      | `A (loc, cnstr) ->
          let new_match = pstring ~loc cnstr --> pexp_variant ~loc cnstr None in
          new_match :: other_matches, false
      | `I inh ->
          handle_variant_inh full_type ~match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_structs `ptag_takes_args
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs in
    handle_variant_match_last loc ~match_last match_atoms_inhs

  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match ~loc ~is_variant cnstr tps =
    let cnstr vars_expr =
      if is_variant
      then pexp_variant ~loc cnstr (Some vars_expr)
      else pexp_construct ~loc (Located.lident ~loc cnstr) (Some vars_expr)
    in
    match tps with
    | [ [%type: [%t? tp] sexp_list ] ] ->
      let cnv = Fun_or_match.expr ~loc (type_of_sexp tp) in
      cnstr [%expr  Sexplib.Conv.list_map ([%e cnv]) sexp_args ]
    | _ ->
      let bindings,patts,good_arg_match =
        let fps = List.map ~f:type_of_sexp tps in
        let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
        let good_arg_match =
          let vars_expr =
            match vars with
            | [var_expr] -> var_expr
            | _ -> pexp_tuple ~loc vars
          in
          cnstr vars_expr
        in
        bindings,patts,good_arg_match
      in
      [%expr
        match sexp_args with
        | [%p plist ~loc patts] ->
          [%e pexp_let ~loc Nonrecursive bindings good_arg_match]
        | _ ->
          [%e
            if is_variant
            then [%expr Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp]
            else [%expr Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp]]
      ]

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, cnstr, tp) ->
        has_structs_ref := true;
        let expr =
          mk_cnstr_args_match ~loc:tp.ptyp_loc ~is_variant:true cnstr [tp]
        in
        let new_match = [%pat? ([%p pstring ~loc cnstr] as _tag)] --> expr in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh full_type ~match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_atoms `ptag_no_args
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    (
      handle_variant_match_last loc ~match_last match_structs_inhs,
      !has_structs_ref
    )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag loc full_type row_field_list =
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:(split_row_field ~loc) ~init:([], [], [], []) row_field_list
    in
    let match_struct, has_structs =
      mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms in
    let maybe_sexp_args_patt =
      if has_structs then [%pat?  sexp_args ]
      else [%pat?  _ ]
    in
    [ [%pat? Sexplib.Sexp.Atom atom as _sexp] -->
      mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs
    ; [%pat? Sexplib.Sexp.List
             (Sexplib.Sexp.Atom atom :: [%p maybe_sexp_args_patt]) as _sexp] -->
      match_struct
    ; [%pat? Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp] -->
      [%expr Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp]
    ; [%pat? Sexplib.Sexp.List [] as sexp] -->
      [%expr Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp]
    ]

  (* Generate matching code for variants *)
  and variant_of_sexp ?full_type (loc,row_fields) =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let top_match =
      match row_fields with
        Rinherit inh :: rest ->
        let rec loop inh row_fields =
          let call =
            [%expr  ( [%e Fun_or_match.expr ~loc (type_of_sexp ~internal:true inh)] sexp :>
                        [%t replace_variables_by_underscores full_type] ) ]
          in
          match row_fields with
          | [] -> call
          | h :: t ->
            let expr =
              match h with
              | Rinherit inh -> loop inh t
              | _ ->
                let rftag_matches =
                  handle_variant_tag loc full_type row_fields
                in
                pexp_match ~loc [%expr sexp] rftag_matches
            in
            pexp_try ~loc call
              (handle_no_variant_match loc expr)
        in
        [ [%pat? sexp] --> loop inh rest ]
      | _ :: _ -> handle_variant_tag loc full_type row_fields
      | [] -> assert false  (* impossible *)
    in
    if is_contained then
      Fun
        [%expr
          fun sexp ->
            try [%e pexp_match ~loc [%expr sexp] top_match]
            with
              Sexplib.Conv_error.No_variant_match ->
              Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
        ]
    else Match top_match

  and poly_of_sexp parms tp =
    let loc = tp.ptyp_loc in
    let bindings =
      let mk_binding parm =
        value_binding ~loc ~pat:(pvar ~loc ("_of_" ^ parm))
          ~expr:[%expr fun sexp -> Sexplib.Conv_error.record_poly_field_value _tp_loc sexp]
      in
      List.map ~f:mk_binding parms
    in
    match type_of_sexp tp with
    | Fun fun_expr -> Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | Match matchings ->
      Match
        [ [%pat? arg] -->
          pexp_let ~loc Nonrecursive bindings
            (pexp_match ~loc [%expr arg] matchings)
        ]

  (* Record conversions *)

  (* Generate code for extracting record fields *)
  let mk_extract_fields (loc,flds) =
    let rec loop no_args args = function
      | {pld_name = {txt=nm; loc};
         pld_type = [%type: sexp_bool ] ; _ } :: more_flds ->
        let no_args =
          (pstring ~loc nm -->
           [%expr
             if ! [%e evar ~loc (nm ^ "_field")] then
               duplicates := ( field_name :: !duplicates )
             else [%e evar ~loc (nm ^ "_field")] := true
           ]
          ) :: no_args
        in
        loop no_args args more_flds
      | {pld_name = {txt=nm; loc};
         pld_type = [%type: [%t? tp] sexp_option ] ; _ } :: more_flds
      | {pld_name = {txt=nm; loc}; pld_type = tp; _ } :: more_flds ->
        let unrolled =
          Fun_or_match.unroll ~loc [%expr  _field_sexp ] (type_of_sexp tp)
        in
        let args =
          (pstring ~loc nm -->
           [%expr
             match ! [%e evar ~loc (nm ^ "_field")] with
             | None ->
               let fvalue = [%e unrolled] in
               [%e evar ~loc (nm ^ "_field")] := Some fvalue
             | Some _ ->
               duplicates := (field_name :: ! duplicates) ]
          ) :: args
        in
        loop no_args args more_flds
      | [] ->
        no_args,args
    in
    let handle_extra =
      [ [%pat? _] -->
        [%expr if !Sexplib.Conv.record_check_extra_fields then
                 extra := (field_name :: !extra)
               else ()]
      ]

    in
    loop handle_extra handle_extra (List.rev flds)

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result has_poly (loc,flds) ~wrap_expr =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop ((res_tpls, bi_lst, good_patts) as acc) = function
        | {pld_name = {txt=nm; loc}; pld_type = tp; _ } as ld :: more_flds ->
          let fld = [%expr ! [%e evar ~loc (nm ^ "_field")]] in
          let mk_default loc =
            bi_lst, [%pat? [%p pvar ~loc (nm ^ "_value")] ] :: good_patts
          in
          let new_bi_lst, new_good_patts =
            match tp with
            | [%type: sexp_bool ]
            | [%type: [%t? _] sexp_option ]
            | [%type: [%t? _] sexp_list ]
            | [%type: [%t? _] sexp_array ]
              -> mk_default loc
            | _ ->
              match Attribute.get Attrs.default ld with
              | Some _ -> mk_default loc
              | None ->
                has_nonopt_fields := true;
                (
                  [%expr
                      (Sexplib.Conv.(=) [%e fld] None, [%e estring ~loc nm]) ] :: bi_lst,
                  [%pat?  Some [%p pvar ~loc (nm ^ "_value")] ] :: good_patts
                )
          in
          let acc =(
            [%expr  [%e fld] ] :: res_tpls,
            new_bi_lst,
            new_good_patts
          )
          in loop acc more_flds
        | [] -> acc
      in
      loop ([], [], []) (List.rev flds)
    in
    let match_good_expr =
      if has_poly then
        let cnvt = function
          | {pld_name = {txt=nm; _}; _ } ->
            evar ~loc (nm ^ "_value")
        in
        match List.map ~f:cnvt flds with
        | [match_good_expr] -> match_good_expr
        | match_good_exprs -> pexp_tuple ~loc match_good_exprs
      else
        let cnvt = function
          | {pld_name = {txt=nm; _};
             pld_type = [%type: [%t? _] sexp_list ]; _ } ->
            (Located.lident ~loc nm),
            [%expr
                match [%e evar ~loc (nm ^ "_value")] with
                | None -> [] | Some v -> v
            ]

          | {pld_name = {txt=nm; _};
             pld_type = [%type: [%t? _] sexp_array ]; _ } ->
            (Located.lident ~loc nm),
            [%expr
                match [%e evar ~loc (nm ^ "_value")] with
                | None -> [||] | Some v -> v
            ]
          | {pld_name = {txt=nm; _}; _ } as ld ->
            begin match Attribute.get Attrs.default ld with
            | None ->
              Located.lident ~loc nm,
              evar ~loc (nm ^ "_value")
            | Some default ->
              Located.lident ~loc nm,
              [%expr
                  match [%e evar ~loc (nm ^ "_value")] with
                    None -> [%e default] | Some v -> v
              ]
            end
        in
        wrap_expr (pexp_record ~loc (List.map ~f:cnvt flds) None)
    in
    let expr, patt =
      match res_tpls, good_patts with
      | [res_expr], [res_patt] -> res_expr, res_patt
      | _ ->
          pexp_tuple ~loc res_tpls,
          ppat_tuple ~loc good_patts
    in
    if !has_nonopt_fields then
      pexp_match ~loc expr
        [ patt --> match_good_expr
        ; [%pat? _] -->
          [%expr
            Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
              [%e elist ~loc bi_lst]
          ]
        ]
    else pexp_match ~loc expr [ patt --> match_good_expr ]

  (* Generate code for converting record fields *)
  let mk_cnv_fields has_poly (loc,flds) ~wrap_expr =
    let field_refs =
      List.map flds ~f:(function
      | {pld_name = {txt=name; loc};
         pld_type = [%type: sexp_bool]; _ } ->
        value_binding ~loc ~pat:(pvar ~loc (name ^ "_field")) ~expr:[%expr ref false]
      | {pld_name = {txt=name; loc}; _ } ->
        value_binding ~loc ~pat:(pvar ~loc (name ^ "_field")) ~expr:[%expr ref None]
      )
    in
    let mc_no_args_fields, mc_fields_with_args = mk_extract_fields (loc,flds) in
    pexp_let ~loc Nonrecursive (field_refs @ [
      value_binding ~loc ~pat:[%pat? duplicates] ~expr:[%expr ref []];
      value_binding ~loc ~pat:[%pat? extra] ~expr:[%expr ref []];
    ]) [%expr
      let rec iter =
        [%e pexp_function ~loc
              [ [%pat?
                       Sexplib.Sexp.List
                       [(Sexplib.Sexp.Atom field_name); _field_sexp] ::
                     tail] -->
                [%expr [%e pexp_match ~loc [%expr field_name] mc_fields_with_args];
                       iter tail]
              ; [%pat? Sexplib.Sexp.List [(Sexplib.Sexp.Atom field_name)] :: tail] -->
                [%expr [%e pexp_match ~loc [%expr field_name] mc_no_args_fields];
                       iter tail]
              ; [%pat? ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _) as sexp) :: _] -->
                [%expr Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp]
              ; [%pat? []] --> [%expr ()]
              ]
        ]
      in
      iter field_sexps;
      match !duplicates with
      | _ :: _ ->
        Sexplib.Conv_error.record_duplicate_fields
          _tp_loc (!duplicates) sexp
      | [] ->
        match !extra with
        | _ :: _ ->
          Sexplib.Conv_error.record_extra_fields _tp_loc (!extra) sexp
        | [] ->
          [%e mk_handle_record_match_result has_poly (loc,flds) ~wrap_expr]
    ]

  let is_poly (_,flds) =
    List.exists flds ~f:(function
    | { pld_type = {ptyp_desc = Ptyp_poly _; _ }; _} -> true
    | _ -> false)

  let label_declaration_list_of_sexp loc flds ~wrap_expr =
    let has_poly = is_poly (loc,flds) in
    let cnv_fields = mk_cnv_fields has_poly (loc,flds) ~wrap_expr in
    if has_poly then
      let patt =
        let pats =
          List.map flds ~f:(fun {pld_name = {txt=name; loc}; _ } ->
            pvar ~loc name
          )
        in
        match pats with
        | [pat] -> pat
        | pats -> ppat_tuple ~loc pats
      in
      let record_def =
        wrap_expr (
          pexp_record ~loc (
            List.map flds ~f:(fun {pld_name = {txt=name; loc}; _ } ->
              (Located.lident ~loc name, evar ~loc name)
            )) None)
      in
      pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:patt ~expr:cnv_fields]
        record_def
    else cnv_fields

  (* Generate matching code for records *)
  let record_of_sexp (loc,flds) : Fun_or_match.t =
    Match
      [ [%pat? Sexplib.Sexp.List field_sexps as sexp] -->
        (label_declaration_list_of_sexp loc flds
           ~wrap_expr:(fun x -> x))
      ; [%pat? Sexplib.Sexp.Atom _ as sexp] -->
        [%expr Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp]
      ]

  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let mk_good_sum_matches (loc,cds) =
    List.map cds ~f:(function
    | { pcd_name = cnstr; pcd_args = Pcstr_record fields; _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      let expr =
        label_declaration_list_of_sexp loc fields
          ~wrap_expr:(fun e ->
            pexp_construct ~loc (Located.lident ~loc cnstr.txt) (Some e))
      in
      [%pat?
             (* Uncomment to wrap record *)
             (* (Sexplib.Sexp.List
              *    [ Sexplib.Sexp.Atom ([%p lcstr] | [%p str] as _tag)
              *    ; Sexplib.Sexp.List field_sexps
              *    ] as sexp) *)
             Sexplib.Sexp.List
               (Sexplib.Sexp.Atom ([%p lcstr] | [%p str] as _tag) :: field_sexps) as sexp
      ] --> expr
    | { pcd_name = cnstr; pcd_args = Pcstr_tuple []; _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? Sexplib.Sexp.Atom ([%p lcstr] | [%p str])] -->
      pexp_construct ~loc (Located.lident ~loc cnstr.txt) None

    | { pcd_name = cnstr; pcd_args = Pcstr_tuple (_::_ as tps); _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? (Sexplib.Sexp.List
                (Sexplib.Sexp.Atom ([%p lcstr] | [%p str] as _tag) ::
                 sexp_args) as _sexp)
      ] -->
      mk_cnstr_args_match ~loc ~is_variant:false cnstr.txt tps
    )

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let mk_bad_sum_matches (loc,cds) =
    List.map cds ~f:(function
    | { pcd_name = cnstr; pcd_args = Pcstr_tuple []; _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? Sexplib.Sexp.List
             (Sexplib.Sexp.Atom ([%p lcstr] | [%p str]) :: _) as sexp
      ] -->
      [%expr Sexplib.Conv_error.stag_no_args _tp_loc sexp]
    | { pcd_name = cnstr; pcd_args = (Pcstr_tuple (_ :: _) | Pcstr_record _); _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? Sexplib.Sexp.Atom ([%p lcstr] | [%p str]) as sexp] -->
      [%expr Sexplib.Conv_error.stag_takes_args _tp_loc sexp]
    )

  (* Generate matching code for sum types *)
  let sum_of_sexp (loc,alts) : Fun_or_match.t =
    Match (List.concat [
      mk_good_sum_matches (loc,alts);
      mk_bad_sum_matches (loc,alts);
      [ [%pat? Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp] -->
        [%expr Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp]
      ; [%pat? Sexplib.Sexp.List [] as sexp] -->
        [%expr Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp]
      ; [%pat? sexp] -->
        [%expr Sexplib.Conv_error.unexpected_stag _tp_loc sexp]
      ]
    ])

  (* Empty type *)
  let nil_of_sexp loc : Fun_or_match.t =
    Fun [%expr  fun sexp -> Sexplib.Conv_error.empty_type _tp_loc sexp ]

  (* Generate code from type definitions *)

  let td_of_sexp ~loc:_ ~poly ~path td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:(fun tp -> (get_type_param_name tp).txt) in
    let {ptype_name = {txt = type_name; loc = _}; ptype_loc = loc; _} = td in
    let full_type =
      core_type_of_type_declaration td
      |> replace_variables_by_underscores
    in
    let is_private = (match td.ptype_private with Private -> true | Public -> false) in
    if is_private
    then Location.raise_errorf ~loc "of_sexp is not supported for private type";
    let create_internal_function =
      match is_polymorphic_variant td ~sig_:false with
      | `Definitely -> true
      | `Maybe -> poly
      | `Surely_not ->
        if poly then
          Location.raise_errorf ~loc
            "sexp_poly annotation on a type that is surely not a polymorphic variant";
        false
    in
    let body =
      let body =
        match td.ptype_kind with
        | Ptype_variant alts -> sum_of_sexp (td.ptype_loc, alts)
        | Ptype_record lbls -> record_of_sexp (loc, lbls)
        | Ptype_open -> Location.raise_errorf ~loc
                          "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          match td.ptype_manifest with
          | None -> nil_of_sexp td.ptype_loc
          | Some { ptyp_desc = Ptyp_variant (rows, _, _); _ } ->
            variant_of_sexp ~full_type (loc, rows)
          | Some ty -> type_of_sexp ~internal:create_internal_function ty
      in
      match body with
      (* Prevent violation of value restriction and problems with
         recursive types by eta-expanding function definitions *)
      | Fun fun_expr -> [%expr fun t -> [%e fun_expr] t ]
      | Match matchings -> pexp_function ~loc matchings
    in
    let external_name = type_name ^ "_of_sexp" in
    let internal_name = "__" ^ type_name ^ "_of_sexp__" in
    let arg_patts, arg_exprs =
      List.unzip (
        List.map ~f:(fun tp ->
            let name = "_of_" ^ tp in
            pvar ~loc name, evar ~loc name)
          tps)
    in
    let bind_tp_loc_in =
      let full_type_name = Printf.sprintf "%s.%s" path type_name in
      (fun e ->
         [%expr
           let _tp_loc = [%e estring ~loc full_type_name] in
           [%e e]])
    in
    let internal_fun_body =
      if create_internal_function
      then Some (bind_tp_loc_in (eabstract ~loc arg_patts body))
      else None
    in
    let external_fun_body =
      let need_tp_loc, body_below_lambdas =
        if create_internal_function
        then
          let no_variant_match_mc =
            [ [%pat? Sexplib.Conv_error.No_variant_match] -->
              [%expr Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp]
            ]
          in
          let internal_call =
            let internal_expr = evar ~loc internal_name in
            eapply ~loc internal_expr (arg_exprs @ [ [%expr sexp] ])
          in
          let try_with = pexp_try ~loc internal_call no_variant_match_mc in
          false, bind_tp_loc_in [%expr fun sexp -> [%e try_with]]
        else
          true, body
      in
      let body_with_lambdas = eabstract ~loc arg_patts body_below_lambdas in
      if need_tp_loc
      then bind_tp_loc_in body_with_lambdas
      else body_with_lambdas
    in
    let typ = Sig_generate_of_sexp.mk_type td in
    let mk_binding func_name body =
      constrained_function_binding loc td typ ~tps ~func_name body
    in
    let internal_bindings =
      match internal_fun_body with
      | None -> []
      | Some body -> [mk_binding internal_name body]
    in
    let external_binding = mk_binding external_name external_fun_body in
    internal_bindings, [external_binding]

  (* Generate code from type definitions *)
  let tds_of_sexp ~loc ~poly ~path (rec_flag, tds) =
    begin
      (* special case for singleton type defs to match camlp4 *)
      let singleton = (match tds with [_] -> true | _ -> false) in
      if singleton
      then
        match really_recursive rec_flag tds with
        | Recursive ->
          let bindings =
            List.map tds ~f:(fun td ->
              let internals,externals = td_of_sexp ~loc ~poly ~path td in
              internals @ externals)
            |> List.concat
          in
          pstr_value_list ~loc Recursive bindings
        | Nonrecursive ->
          let bindings =
            List.map tds ~f:(fun td ->
              let internals,externals = td_of_sexp ~loc ~poly ~path td in
              pstr_value_list ~loc Nonrecursive internals @
              pstr_value_list ~loc Nonrecursive externals
            )
            |> List.concat
          in
          bindings
      else
        let bindings =
          List.map tds ~f:(fun td ->
            let internals,externals = td_of_sexp ~poly ~loc ~path td in
            internals @ externals)
          |> List.concat
        in
        pstr_value_list ~loc rec_flag bindings
    end

  let type_of_sexp ~path ctyp =
    let loc = ctyp.ptyp_loc in
    let fp = type_of_sexp ctyp in
    let body =
      match fp with
      | Fun fun_expr    -> [%expr  [%e fun_expr] sexp ]
      | Match matchings -> pexp_match ~loc [%expr sexp] matchings
    in
    let full_type_name =
      Printf.sprintf "%s line %i: %s"
        path loc.loc_start.pos_lnum
        (string_of_core_type ctyp)
    in
    [%expr
      fun sexp ->
        let _tp_loc = [%e estring ~loc full_type_name] in
        [%e body]
    ]
  ;;
end
     *)


module TypeNameMap = Map.M(String)

module FoldInfo = struct
  type item = {param_name:string; rtyp: core_type; ltyp: core_type}
  exception ItemFound of item
  type t = item list

  let param_for_rtyp typ ts =
    let typ_repr =
      Pprintast.core_type Caml.Format.str_formatter typ;
      Caml.Format.flush_str_formatter ()
    in
    try List.iter ts (fun i ->
                    let new_repr : string =
                      Pprintast.core_type Caml.Format.str_formatter i.rtyp;
                      Caml.Format.flush_str_formatter ()
                    in
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

let extract_names = List.map ~f:(fun typ ->
    match typ.ptyp_desc with
    | Ptyp_var s -> s
    | _ ->
      Buffer.clear Caml.Format.stdbuf;
      Pprintast.core_type Caml.Format.str_formatter typ;
      failwith (Printf.sprintf "Don't know what to do with %s" (Caml.Format.flush_str_formatter ()))
  )

(* let str_type_ ?loc flg = pstr_type ~loc flg  *)

let nolabel =
      Asttypes.Nolabel

let get_param_names pcd_args =
  let Pcstr_tuple pcd_args  = pcd_args in
  extract_names pcd_args

let mangle_construct_name name =
  let low = String.mapi ~f:(function 0 -> Char.lowercase | _ -> fun x -> x ) name in
  match low with
  | "val" | "if" | "else" | "for" | "do" | "let" | "open" | "not" -> low ^ "_"
  | _ -> low

let lower_lid lid = Location.{lid with txt = mangle_construct_name lid.Location.txt }

module Location = struct
  include Location
  let mknoloc txt = { txt; loc = none }
end

let prepare_distribs ~loc tdecl fmap_decl =
  let open Location in
  let open Longident in
  let Ptype_variant constructors = tdecl.ptype_kind in

  let gen_module_str = Location.mknoloc @@ "For_" ^ tdecl.ptype_name.txt in
  let distrib_lid = mknoloc Longident.(Ldot (Lident gen_module_str.txt, "distrib")) in
  [ pstr_module ~loc @@ module_binding ~loc ~name:(Location.mknoloc "T")
     ~expr:(pmod_structure ~loc
        [ fmap_decl
        ; pstr_type ~loc Nonrecursive
            [ type_declaration ~loc
                ~params:tdecl.ptype_params
                ~kind:Ptype_abstract
                ~private_: Public
                ~cstrs:[] (* TODO: something is wrong here *)
                ~manifest:(Some (ptyp_constr ~loc (mknoloc @@ Lident tdecl.ptype_name.txt) @@ List.map ~f:fst tdecl.ptype_params))
                ~name:(mknoloc "t") ]
        ])
  ; pstr_module ~loc @@ module_binding ~loc ~name:gen_module_str
     ~expr:(pmod_apply ~loc
        (pmod_ident ~loc (mknoloc @@ Lident (match tdecl.ptype_params with [] -> "Fmap" | xs -> sprintf "Fmap%d" (List.length xs)) ))
        (pmod_ident ~loc (mknoloc @@ Lident "T")) )
  ] @ (List.map constructors ~f:(fun {pcd_name; pcd_args} ->
      let names = get_param_names pcd_args |> List.mapi ~f:(fun i _ -> sprintf "a%d" i) in
      let body =
        let constr_itself args = econstruct (constructor_declaration ~loc ~name:(mknoloc pcd_name.txt) ~res:None ~args:pcd_args) args in
        match names with
        | [] -> constr_itself None
        | [one] -> constr_itself (Some (pexp_ident ~loc @@ mknoloc (Lident one)))
        | xs -> (* construct tuple here *)
            constr_itself (Some (pexp_tuple ~loc @@ List.map ~f:(fun name -> pexp_ident ~loc @@ mknoloc (Lident name)) xs))
      in
      let body = [%expr inj [%e pexp_apply ~loc (pexp_ident ~loc distrib_lid) [nolabel, body] ] ] in
      pstr_value ~loc Asttypes.Nonrecursive [
      value_binding ~loc ~pat:(ppat_var ~loc @@ lower_lid pcd_name)
        ~expr:(match names with
        | [] -> pexp_fun ~loc nolabel None (ppat_construct ~loc (mknoloc (Lident "()")) None) body
        | names -> List.fold_right ~f:(fun name acc -> pexp_fun ~loc nolabel None (ppat_var ~loc @@ mknoloc name) acc) names ~init:body)
    ]
    )
    )

let prepare_fmap ~loc tdecl =
  let open Location in
  let open Ast_helper in

  let param_names = extract_names (List.map ~f:fst tdecl.ptype_params) in
  let Ptype_variant constructors = tdecl.ptype_kind in
  let cases = constructors |> List.map ~f:(fun {pcd_name; pcd_args} ->
    let argnames = get_param_names pcd_args in
    let cname = pcd_name.txt in
    let clid = mknoloc @@ Longident.Lident cname in
    let make_f_expr name = pexp_ident ~loc @@ mknoloc @@ Longident.Lident ("f"^name) in
    let pc_lhs, pc_rhs =
      let wrap_one_arg typname new_name =
        pexp_apply ~loc (make_f_expr typname) [nolabel, pexp_ident ~loc @@ mknoloc @@ Longident.Lident new_name]
      in
      let get_pat_name i name = sprintf "%s_%d" name i in
      match argnames with
      | [] -> (None, None)
      | [s] -> Some (ppat_var ~loc (mknoloc s)), (Some (wrap_one_arg s s))
      | ___ ->
          Some (ppat_tuple ~loc @@ List.mapi ~f:(fun n name -> ppat_var ~loc (mknoloc @@ get_pat_name n name)) argnames),
          Some (pexp_tuple ~loc @@ List.mapi ~f:(fun n name -> wrap_one_arg name @@ get_pat_name n name) argnames)
    in
    let pc_lhs = Ast_helper.Pat.construct clid pc_lhs in
    let pc_rhs = Exp.construct clid pc_rhs in
    let pc_guard = None in
    { pc_rhs; pc_lhs; pc_guard }
  )
  in

  [%stri let rec fmap = [%e
    List.fold_right ~f:(function
      | name -> pexp_fun ~loc nolabel None (ppat_var ~loc @@ mknoloc ("f"^name))
      ) param_names ~init:(Exp.function_ cases) ] ]

let revisit_adt ~loc tdecl ctors =
  let der_typ_name = tdecl.ptype_name.Asttypes.txt in
  (* Let's forget about mutal recursion for now *)
  (* For every constructor argument we need to put ground types to parameters *)
  let mapa, full_t =
    List.fold_right
      ~f:(fun cd (n, acc_map,cs) ->
          let n,map2,new_args = List.fold_right
            ~f:(fun typ (n, map,args) ->
                  match typ with
                  | [%type: _] -> assert false
                  | {ptyp_desc = Ptyp_var s; _} -> (n, map, typ::args)
                  | arg ->
                      match FoldInfo.param_for_rtyp arg map with
                      | Some {param_name; } -> (n, map, (ptyp_var ~loc param_name)::args)
                      | None ->
                          let new_name = sprintf "a%d" n in
                          (n+1, FoldInfo.extend new_name arg arg map, (ptyp_var ~loc new_name)::args)
              )
            (match cd.pcd_args with Pcstr_tuple tt -> tt | Pcstr_record _ -> assert false)
            ~init:(n, acc_map,[])
          in
          let new_args = Pcstr_tuple new_args in
          (n, map2, { cd with pcd_args = new_args } :: cs)
      )
      ctors
      ~init:(0, FoldInfo.empty, [])
      |> (fun (_, mapa, cs) -> mapa, {tdecl with ptype_kind = Ptype_variant cs})
  in
  (* now we need to add some parameters if we collected ones *)
  let functor_typ, typ_to_add =
    if FoldInfo.is_empty mapa
    then
      let fmap_for_typ = prepare_fmap ~loc full_t in
      None, (*(str_type_ ~loc Nonrecursive [full_t]) ::*) (prepare_distribs ~loc full_t fmap_for_typ)
    else
      let functor_typ =
        let extra_params = FoldInfo.map mapa
          ~f:(fun fi -> (Ast_helper.Typ.var fi.FoldInfo.param_name, Asttypes.Invariant))
        in
        let open Location in
        {full_t with ptype_params = full_t.ptype_params @ extra_params;
                     ptype_name = { full_t.ptype_name with txt = "g" ^ full_t.ptype_name.txt }}
      in
      let fmap_for_typ = prepare_fmap ~loc functor_typ in
      (Some functor_typ), ([fmap_for_typ] @ (prepare_distribs ~loc functor_typ fmap_for_typ))
  in
(*      let non_logic_typ =
        let alias_desc =
          let old_params = List.map fst tdecl.ptype_params  in
          let extra_params = FoldInfo.map ~f:(fun {FoldInfo.rtyp} -> rtyp)  mapa in
          Ptyp_constr (Location.mknoloc (Longident.Lident functor_typ.ptype_name.Asttypes.txt), old_params @ extra_params)
        in
        str_type_ ~loc Recursive
          [ { tdecl with ptype_kind = Ptype_abstract
            ; ptype_manifest = Some { ptyp_loc = Location.none; ptyp_attributes = []; ptyp_desc = alias_desc}
            } ]
     in*)
  (match functor_typ with Some functor_typ -> [ pstr_type ~loc Nonrecursive [functor_typ]] | None -> [])
   @ typ_to_add

let has_to_gen_attr (xs: attributes) =
  try let _ = List.find ~f:(fun (name,_) -> String.equal name.Location.txt "distrib") xs in
      true
  with Not_found -> false

let suitable_tydecl_wrap ~on_ok ~on_fail tdecl =
  match tdecl.ptype_kind with
  | Ptype_variant cs when Option.is_none tdecl.ptype_manifest &&
                          has_to_gen_attr tdecl.ptype_attributes ->
    Attribute.explicitly_drop#type_declaration tdecl;
    on_ok cs { tdecl with ptype_attributes = [] }
  | _ -> on_fail ()

let suitable_tydecl =
  suitable_tydecl_wrap ~on_ok:(fun _ _ -> true) ~on_fail:(fun () -> false)

let str_type_decl ~loc (flg,tdls) =
  let wrap_tydecls loc ts =
    let f tdecl =
      suitable_tydecl_wrap tdecl
        ~on_ok:(fun cs tdecl -> revisit_adt ~loc tdecl cs)
        ~on_fail:(fun () ->
            failwith "Only variant types without manifest are supported")
    in
    List.concat (List.map ~f ts)
  in
  wrap_tydecls loc tdls


