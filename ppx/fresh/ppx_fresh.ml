(*
 * OCanren. PPX suntax extensions.
 * Copyright (C) 2015-2019
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

(*   Performs two minikanren-specific macro expansions:
 *  1) fresh (x1 ... xm) e1 ... en
 *            to
 *     Fresh.numeral (fun x1 ... xm -> e1 &&& ... &&& en)
 *
 *  2) TODO: write about defer
 *)

open Base
open Ppxlib
open Ppxlib.Ast_helper

let is_state_pattern pat =
  match pat.ppat_desc with
  | Ppat_var v when String.equal v.txt "st" || String.equal v.txt "state" -> Some v.txt
  | _ -> None

let classify_name ~f e =
  match e.pexp_desc with
  | Pexp_ident i when f i.txt -> true
  | _ -> false
let need_insert_fname ~name e =
  classify_name e ~f:(Stdlib.(=) (Lident name))
  (* match e.pexp_desc with
  | Pexp_ident i when i.txt = Lident name -> true
  | _ -> false *)

let is_defer = need_insert_fname ~name:"defer"
let is_conde = need_insert_fname ~name:"conde"
let is_fresh = need_insert_fname ~name:"fresh"
let is_call_fresh = need_insert_fname ~name:"call_fresh"
let is_unif =
  classify_name
    ~f:(function
        | Lident s -> (String.length s >=3) && (String.equal (String.sub s ~pos:0 ~len:3) "===")
        | _ -> false
        )

let is_conj = need_insert_fname ~name:"conj"
let is_conj_list = need_insert_fname ~name:"?&"

let is_disj e =
  need_insert_fname ~name:"disj" e || need_insert_fname ~name:"|||" e

(*
let rec walkthrough ~fname (expr: expression) =

  let add_fname () =
    [%expr [%e Ast_helper.Exp.constant (Pconst_string (fname,None))] <=>
           [%e expr]
    ]
  in
  match expr.pexp_desc with
  | Pexp_fun (_label, _opt, pat, e2) -> begin
      match is_state_pattern pat with
      | None ->
        { expr with pexp_desc =
                   Pexp_fun (_label, _opt, pat, walkthrough ~fname e2) }
      | Some argname ->
        (* printf "found good function with statearg '%s'\n%!" argname; *)
        let new_body =
          [%expr
             let () = Printf.printf "entering '%s'\n%!" [%e Ast_helper.Exp.constant (Pconst_string (fname,None))] in
             let ans = [%e e2] in
             let () = Printf.printf "leaving '%s'\n%!"  [%e Ast_helper.Exp.constant (Pconst_string (fname,None))] in
             ans
          ]
        in
        { expr with pexp_desc= Pexp_fun (_label, _opt, pat, new_body) }
    end
  | Pexp_apply (e,_) when is_call_fresh e -> add_fname ()
  | Pexp_apply (e,_) when is_disj e -> add_fname ()
  | Pexp_apply (e,_) when is_conj e -> add_fname ()

  | _ -> expr


let map_value_binding (vb : value_binding) =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name ->
    let fname = name.txt in
    { vb with pvb_expr = walkthrough ~fname vb.pvb_expr }
  |  _ -> vb

let smart_logger =
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec, vbs) ->
        { sitem with pstr_desc = Pstr_value (_rec, List.map vbs ~f:map_value_binding) }
      | x -> default_mapper.structure_item mapper sitem
  }
*)

let option_map ~f = function Some x -> Some (f x) | None -> None
let option_bind ~f = function Some x -> f x | None -> None

exception Not_an_ident
let reconstruct_args e =
  let open Longident in
  let are_all_idents (xs: (_*expression) list) =
    try Some (List.map xs ~f:(fun (_,e) ->
                        match e.pexp_desc with
                        | Pexp_ident {txt=(Longident.Lident i);_} -> i
                        | _ -> raise Not_an_ident))
    with Not_an_ident -> None
  in
  match e.pexp_desc with
  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Longident.Lident arg1; _}}, ys) ->
     (* fresh (var1 var2 var3) body *)
      option_map (are_all_idents ys) ~f:(fun xs -> arg1::xs )

  (* no fresh variables: just for geting rid of &&&  *)
  | Pexp_construct ({ txt=Lident "()" }, None) -> Some []
  (* [fresh arg0 body] -- single fresh variable  *)
  | Pexp_ident {txt=Lident arg1; _} ->  Some [arg1]

  | _ -> None


let list_fold ~f ~initer xs =
  match xs with
  | [] -> failwith "bad argument"
  | start::xs -> List.fold ~init:(initer start) ~f xs

let list_fold_right0 ~f ~initer xs =
  let rec helper = function
  | [] -> failwith "bad_argument"
  | x::xs -> list_fold ~initer ~f:(fun acc x -> f x acc) (x::xs)
  in
  helper (List.rev xs)

let my_list ~loc es =
  List.fold_right ~init:[%expr []] es ~f:(fun x acc -> [%expr [%e x] :: [%e acc]])

let parse_to_list alist =
  let rec helper acc = function
  | Pexp_construct ({txt=Lident "[]"}, None ) -> acc
  | Pexp_construct ({txt=Lident "::"}, Some {pexp_desc=Pexp_tuple [y1;y2]; _})
      ->
      helper (y1::acc) y2.pexp_desc
  | _ -> []
  in
  List.rev @@ helper [] alist

let mapper = object(self)
  inherit Ast_traverse.map as super

  method! expression e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    | Pexp_apply (_,[]) -> e
    | Pexp_apply (e1,(_,alist)::args) when is_conj_list e1 ->
        let clauses : expression list = parse_to_list alist.pexp_desc in
        let ans = list_fold_right0 clauses
          ~initer:(fun x -> x)
          ~f:(fun x acc -> [%expr [%e x] &&& [%e acc]])
        in
        super#expression ans
    | Pexp_apply (e1,(_,alist)::otherargs) when is_conde e1 ->
        let clauses : expression list = parse_to_list alist.pexp_desc in
        [%expr
          conde [%e my_list ~loc @@ List.map ~f:self#expression clauses ]
        ]
    | Pexp_apply (e1,[args]) when is_fresh e1 ->
        (* bad syntax -- no body*)
        e
    | Pexp_apply (e1, (Nolabel,args) :: body) when is_fresh e1 -> begin
        assert (List.length body > 0);
        let body = List.map ~f:snd body in

        let new_body : expression =
          match body with
          | [] -> assert false
          | [body] -> self#expression body
          | body ->
              let xs = List.map ~f:self#expression body in
              [%expr ?& [%e my_list ~loc xs ] ]
        in
        match reconstruct_args args with
        | Some (xs: string list) ->
            let ans =
              List.fold_right xs
                ~f:(fun ident acc ->
                    [%expr
                      Fresh.one (fun [%p Pat.var ~loc (Ast_builder.Default.Located.mk ident ~loc) ] -> [%e acc])
                    ]
                  )
                ~init:[%expr delay (fun () -> [%e new_body ])]
            in
            ans
        | None ->
          Caml.Format.eprintf "Can't reconstruct args of 'fresh'";
          {e with pexp_desc=Pexp_apply (e1,[Nolabel, new_body]) }
      end
    | Pexp_apply (d, [(_,body)]) when is_defer d ->
        let ans = [%expr delay (fun () -> [%e self#expression body])] in
        ans
    | Pexp_apply (d, body) when is_unif d ->
        (* let loc_str =
          Caml.Format.asprintf "%a" Selected_ast.Ast.Location.print_compact e.pexp_loc;
        in
        let body = (Labelled "loc", Exp.constant (Pconst_string (loc_str,None))) :: body in *)
        Exp.apply ~loc:e.pexp_loc d body

    | Pexp_apply (e, xs) ->
        let ans = Pexp_apply (self#expression e,
                              List.map ~f:(fun (lbl,e) -> (lbl, self#expression e)) xs ) in
        let ans = {e with pexp_desc = ans} in
        ans
    | Pexp_fun (l,opt,pat,e) ->
        { e with pexp_desc = Pexp_fun(l, opt, pat, self#expression e) }

    | Pexp_construct (_, None) -> e
    | Pexp_construct (id, Some e1) -> { e with pexp_desc = Pexp_construct (id, Some (self#expression e1)) }

    (* kind of default mapping below *)

    | Pexp_constant _
    | Pexp_ident _ -> e
    | Pexp_variant (l, eopt) ->
        let eopt = option_map eopt ~f:self#expression  in
        { e with pexp_desc = Pexp_variant (l, eopt) }
    | Pexp_record (xs, o) ->
        let o = option_map o ~f:self#expression in
        let xs = List.map xs ~f:(fun (s, e) -> (s, self#expression e)) in
        { e with pexp_desc = Pexp_record (xs, o) }
    | Pexp_field (e, lident) ->
        let e = self#expression e in
        { e with pexp_desc = Pexp_field (e, lident) }
    | Pexp_setfield (l, lab, r) ->
        let l = self#expression l in
        let r = self#expression r in
        { e with pexp_desc = Pexp_setfield (l, lab, r) }
    | Pexp_array es ->
        { e with pexp_desc=Pexp_array (List.map ~f:self#expression es) }
    | Pexp_ifthenelse (s, th, el) ->
        let s = self#expression s in
        let th = self#expression th in
        let el = option_map el ~f:self#expression in
        { e with pexp_desc = Pexp_ifthenelse (s, th, el) }
    | Pexp_sequence (e1, e2) ->
        { e with pexp_desc = Pexp_sequence (self#expression e1, self#expression e2) }
    | Pexp_tuple es ->
        { e with pexp_desc=Pexp_tuple (List.map ~f:self#expression es) }
    | Pexp_let   (_recflag, vbs,where_expr) ->
        let vbs_new = List.map vbs ~f:(fun vb -> {vb with pvb_expr=(self#expression vb.pvb_expr)}) in
        { e with pexp_desc = Pexp_let(_recflag, vbs_new, self#expression where_expr) }

    | Pexp_while (e1, e2) ->
        let e1 = self#expression e1 in
        let e2 = self#expression e2 in
        { e with pexp_desc = Pexp_while (e1, e2) }
    | Pexp_for (p, e1, e2, flg, e3) ->
        let e1 = self#expression e1 in
        let e2 = self#expression e2 in
        let e3 = self#expression e3 in
        { e with pexp_desc = Pexp_for (p, e1, e2, flg, e3) }
    | Pexp_constraint (ee,t) ->
        { e with pexp_desc = Pexp_constraint(self#expression ee, t) }
    | Pexp_coerce (expr, t1, t2) ->
        let expr = self#expression expr in
        { e with pexp_desc = Pexp_coerce (expr, t1, t2) }

    | Pexp_send (e, lab) ->
        let e = self#expression e in
        { e with pexp_desc = Pexp_send (e, lab) }
    | Pexp_new _ -> e
    | Pexp_setinstvar (l, body) ->
        let body = self#expression body in
        { e with pexp_desc = Pexp_setinstvar (l, body) }

    | Pexp_override es ->
        let es = List.map es ~f:(fun (l,e) -> (l, self#expression e)) in
        { e with pexp_desc = Pexp_override es }

    | Pexp_letmodule (name, me, body) ->
        { e with pexp_desc = Pexp_letmodule (name, self#module_expr me, self#expression body) }
    | Pexp_letexception (ec, e1) ->
        let e1 = self#expression e1 in
        { e with pexp_desc = Pexp_letexception (ec, e1) }
    | Pexp_assert e ->
        { e with pexp_desc = Pexp_assert (self#expression e) }
    | Pexp_lazy e1 ->
        let e1 = self#expression e1 in
        { e with pexp_desc = Pexp_lazy e1 }
    | Pexp_poly (e1,t) ->
        let e1 = self#expression e1 in
        { e with pexp_desc = Pexp_poly (e1,t) }
    | Pexp_newtype (name, ee) ->
        { e with pexp_desc=Pexp_newtype(name, self#expression ee) }
    | Pexp_function cases -> { e with pexp_desc = Pexp_function (List.map ~f:self#case cases) }
    | Pexp_match (s, cases) ->
        let scru = self#expression s in
        { e with pexp_desc = Pexp_match (scru, List.map ~f:self#case cases) }
    | Pexp_try (s, cases) ->
        let scru = self#expression s in
        { e with pexp_desc = Pexp_try (scru, List.map ~f:self#case cases) }
    | Pexp_object _
    | Pexp_unreachable -> e
    | Pexp_open (_od, ee) ->
        { e with pexp_desc=Pexp_open (_od, self#expression ee) }
    | Pexp_letop _
    | Pexp_extension _
    | Pexp_pack _ -> e
(*    | _ ->
      Caml.Format.printf "%a\n%a\n%!" Location.print loc Pprintast.expression e;
      assert false*)
end

let () =
  Ppxlib.Driver.register_transformation
    ~impl:mapper#structure
    "pa_minikanren"
