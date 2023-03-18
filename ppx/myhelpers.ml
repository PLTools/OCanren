(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren PPX
 * Copyright (C) 2016-2023
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Ppxlib
open Stdppx

let lident_of_list = function
  | [] -> failwith "Bad argument: lident_of_list"
  | s :: tl -> List.fold_left tl ~init:(Lident s) ~f:(fun acc x -> Ldot (acc, x))
;;

(* TODO: maybe use Ppxlib.name_type_params_in_td ? *)

let extract_names =
  List.map ~f:(fun (typ, _) ->
    match typ.ptyp_desc with
    | Ptyp_var s -> s
    | _ ->
      failwith
        (Caml.Format.asprintf "Don't know what to do with %a" Pprintast.core_type typ))
;;

open Ppxlib.Ast_builder.Default
open Ppxlib.Ast_helper

module Located = struct
  include Located

  (* let mknoloc txt = { txt; loc = Location.none } *)
  let map_loc ~f l = { l with txt = f l.txt }
  let sprintf ~loc fmt = Caml.Format.kasprintf (mk ~loc) fmt
end

module Exp = struct
  include Exp

  let mytuple ~loc ?(attrs = []) = function
    (* | [] -> Exp.construct (Located.mk ~loc (lident "()")) None *)
    | [] -> failwith "Bad argument: mytuple"
    | [ x ] -> x
    | xs -> tuple ~loc ~attrs xs
  ;;

  let apply ~loc f = function
    | [] -> f
    | xs -> apply ~loc f (List.map ~f:(fun e -> Nolabel, e) xs)
  ;;

  let funs ~loc body xs =
    match xs with
    | [] -> body
    | xs ->
      List.fold_right xs ~init:body ~f:(fun n acc ->
        pexp_fun ~loc Nolabel None (ppat_var ~loc (Located.mk ~loc n)) acc)
  ;;

  let lident ~loc l = pexp_ident ~loc (Located.mk ~loc (lident l))
  let ident ~loc lident = pexp_ident ~loc (Located.mk ~loc lident)
end

let lident_of_list = function
  | [] -> failwith "Bad argument: lident_of_list"
  | s :: tl -> List.fold_left tl ~init:(Lident s) ~f:(fun acc x -> Ldot (acc, x))
;;

let failwiths ?(loc = Location.none) fmt = Location.raise_errorf ~loc fmt

let notify fmt =
  Format.kasprintf
    (fun s -> ignore (Sys.command (Printf.sprintf "notify-send %S" s)))
      (* (fun s ->
      assert (
        0 = Sys.command (Printf.sprintf "dunstify --action='replyAction,reply' %S" s))) *)
    fmt
;;
