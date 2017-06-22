open Migrate_parsetree
open Ast_405
module Ast_convenience = Ast_convenience_405

open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open StdLabels
open Ast_convenience

module Exp = struct
  include Ast_helper.Exp
  let const_string s = Exp.constant @@ Pconst_string (s, None)
end

let is_state_pattern pat =
  match pat.ppat_desc with
  | Ppat_var v when v.txt = "st" || v.txt = "state" -> Some v.txt
  | _ -> None

let classify_name ~f e =
  match e.pexp_desc with
  | Pexp_ident i when f i.txt -> true
  | _ -> false
let need_insert_fname ~name e =
  classify_name e ~f:((=) (Lident name))
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
        | Lident s -> (String.length s >=3) && (String.sub s 0 3 = "===")
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
  let are_all_idents (xs: (_*expression) list) =
    try Some (List.map (fun (_,e) ->
                        match e.pexp_desc with
                        | Pexp_ident {txt=(Longident.Lident i);_} -> i
                        | _ -> raise Not_an_ident) xs)
    with Not_an_ident -> None
  in
  match e.pexp_desc with
  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Longident.Lident arg1; _}}, ys) ->
     (* fresh (var1 var2 var3) body *)
      option_map (are_all_idents ys) ~f:(fun xs -> arg1::xs )

  | Pexp_ident {txt=Longident.Lident arg1; _} ->
     (* fresh arg0 body *)
     Some [arg1]
  | _ -> None

let list_fold ~f ~initer xs =
  match xs with
  | [] -> failwith "bad argument"
  | start::xs -> List.fold_left ~init:(initer start) ~f xs

let list_fold_right0 ~f ~initer xs =
  let rec helper = function
  | [] -> failwith "bad_argument"
  | x::xs -> list_fold ~initer ~f:(fun acc x -> f x acc) (x::xs)
  in
  helper (List.rev xs)

let my_list es =
  List.fold_right ~init:[%expr []] es ~f:(fun x acc -> [%expr [%e x] :: [%e acc]])

let parse_to_list alist =
  let rec helper acc = function
  | Pexp_construct (loc, None ) when loc.txt = Lident "[]" -> acc
  | Pexp_construct (loc, Some {pexp_desc=Pexp_tuple [y1;y2]; _})
      when loc.txt = Lident "::" ->
      helper (y1::acc) y2.pexp_desc
  | _ -> []
  in
  List.rev @@ helper [] alist

let rec pamk_e ?(need_st=false) mapper e : expression =
  (* Printast.expression 0 Format.std_formatter e; *)
  match e.pexp_desc with
  | Pexp_apply (_,[]) -> e
  | Pexp_apply (e1,(_,alist)::args) when is_conj_list e1 ->
      let clauses : expression list = parse_to_list alist.pexp_desc in
      let ans = list_fold_right0 clauses
        ~initer:(fun x -> x)
        ~f:(fun x acc -> [%expr [%e x] &&& [%e acc]])
      in
      pamk_e ~need_st mapper ans
  | Pexp_apply (e1,(_,alist)::otherargs) when is_conde e1 ->
      let clauses : expression list = parse_to_list alist.pexp_desc in
      let ans =
      [%expr
        mylog (fun () -> printfn " creating inc in conde");
        MKStream.inc (fun () ->
          let st = State.incr_scope st in
          mylog (fun () -> printfn " force a conde");
          [%e
          match clauses with
          | [] -> failwith "conde with no clauses is a nonsense"
          (* | [b] ->
            (* failwith "conde with a single clause appear very rarely" *)
            pamk_e ~need_st mapper b *)
          | clauses ->
            let wrap need_st =
              list_fold_right0 clauses
                ~initer:(pamk_e ~need_st mapper)
                ~f:(fun x acc ->
                  [%expr
                    MKStream.mplus [%e pamk_e ~need_st mapper x]
                      (MKStream.inc (fun () ->
                        mylog (fun () -> printfn " force inc from mplus*");
                        [%e acc]))
                  ]
                )
            in
            wrap true
          ]
        )
      ]
      in
      let ans = if need_st then ans
         else [%expr fun st -> [%e ans]]
      in
      if otherargs <> []
      then Exp.apply ans otherargs
      else ans
  | Pexp_apply (e1,[args]) when is_fresh e1 ->
      (* bad syntax -- no body*)
     e
  | Pexp_apply (e1, (_,args) :: body) when is_fresh e1 -> begin
      (* List.iter body ~f:(fun (_,e) -> Printast.expression 0 Format.std_formatter e); *)
      assert (List.length body > 0);
      let body = List.map snd body in

      let new_body : expression =
        match body with
        | [] -> assert false
        | [body] ->
            (* we omitte bind* here*)
            pamk_e ~need_st:true mapper body
        | body ->
            let xs = List.map (pamk_e ~need_st:false mapper) body in
            [%expr ?& [%e Ast_convenience.list xs ] st ]
      in
      match reconstruct_args args with
      | Some (xs: string list) ->
          let pretty_names = StringLabels.concat ~sep:" " xs in
          let msg2 = sprintf "create inc in fresh ==== (%s)" pretty_names in
          let msg3 = sprintf "inc in fresh forced: (%s)" pretty_names in
          let ans =
            List.fold_right xs
              ~f:(fun ident acc ->
                  let msg = sprintf "create new variable %s as" ident in
                  let msg = msg ^ " _.%d" in
                  [%expr
                    let [%p pvar ident ], idx = State.new_var st in
                    mylog (fun () -> printfn [%e  Exp.const_string msg] idx);
                    [%e acc]
                  ]
                 )
              ~init:[%expr
                mylog (fun () -> printfn [%e Exp.const_string msg2]);
                MKStream.inc (fun () ->
                  mylog (fun () -> printfn [%e Exp.const_string msg3]);
                  [%e new_body ]
                )]
          in

          if need_st then ans
          else [%expr fun st -> [%e ans]]
      | None ->
         eprintf "Can't reconstruct args of 'fresh'";
         {e with pexp_desc=Pexp_apply (e1,[Nolabel, new_body]) }
    end
  | Pexp_apply (d, [(_,body)]) when is_defer d ->
      let ans = [%expr MKStream.inc (fun () -> [%e pamk_e ~need_st:false mapper body])] in
      if need_st
      then [%expr [%e ans] st]
      else ans
  | Pexp_apply (d, body) when is_unif d ->

      let loc_str =
        let b = Buffer.create 10 in
        let fmt = Format.formatter_of_buffer b in
        Location.print_compact fmt e.pexp_loc;
        Format.pp_flush_formatter fmt;
        Buffer.contents b
      in
      (* let ans = e in *)
      let body = (Labelled "loc", Exp.const_string loc_str) :: body in
      let ans = Exp.apply ~loc:e.pexp_loc d body in
      if need_st then [%expr [%e ans ] st]
      else ans
  | Pexp_apply (e, xs) ->
      let ans = Pexp_apply (mapper.expr mapper e,
                                   List.map ~f:(fun (lbl,e) -> (lbl, mapper.expr mapper e)) xs ) in
      let ans = {e with pexp_desc = ans} in
      if need_st then [%expr [%e ans] st]
      else ans
  | Pexp_fun (l,opt,pat,e) ->
     { e with pexp_desc=Pexp_fun(l,opt,pat, mapper.expr mapper e) }

  | Pexp_construct (_, None) -> e
  | Pexp_construct (id, Some e1) -> { e with pexp_desc = Pexp_construct (id, Some (mapper.expr mapper e1)) }

  | Pexp_tuple es -> {e with pexp_desc=Pexp_tuple (List.map (mapper.expr mapper) es) }
  | Pexp_let   (_recflag, vbs,where_expr) ->
     let vbs_new = List.map (fun vb -> {vb with pvb_expr=mapper.expr mapper vb.pvb_expr}) vbs
     in
     {e with pexp_desc=Pexp_let(_recflag, vbs_new, mapper.expr mapper where_expr) }
  | Pexp_sequence (e1, e2) ->
     {e with pexp_desc=Pexp_sequence(mapper.expr mapper e1,mapper.expr mapper e2) }
  | Pexp_open (_flag, _loc, ee) ->
     { e with pexp_desc=Pexp_open (_flag, _loc, mapper.expr mapper ee) }
  | Pexp_newtype (name, ee) ->
     { e with pexp_desc=Pexp_newtype(name, mapper.expr mapper ee) }
  | Pexp_constraint (ee,t) ->
     { e with pexp_desc=Pexp_constraint(mapper.expr mapper ee, t) }
  (* TODO: support all cases *)
  | _ -> e


let pa_minikanren =
  { default_mapper with expr = fun mapper e -> pamk_e mapper e
  }

let register () = Driver.register ~name:"pa_minikanren" Versions.ocaml_405 (fun _ _ -> pa_minikanren)
