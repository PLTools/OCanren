(*
  Do not use it yet!

  converts code like
    val x : int * int * int [@@noinjected]
  into
    val x :
      (asdf, qwerty) injected ->
        (asdf, qwerty) injected -> (asdf, qwerty) injected -> goal

  The main issue is to select right mangling of types
*)

open Ppxlib
open Stdppx
open Ppxlib.Ast_builder.Default
open Printf

let name = "noinjected"

exception AttrFound of attribute

let find_good_attribute attrs =
  try
    List.iter attrs ~f:(fun ({ attr_name } as attr) ->
        if String.equal attr_name.txt name then raise (AttrFound attr));
    None
  with
  | AttrFound attr -> Some attr
;;

let obj =
  object (self)
    inherit Ast_traverse.map as super

    method! signature_item ss =
      match ss.psig_desc with
      | Psig_value vd ->
        (match find_good_attribute vd.pval_attributes with
        | None -> ss
        | Some attr ->
          Attribute.explicitly_drop#value_description vd;
          let new_typ =
            match vd.pval_type.ptyp_desc with
            | Ptyp_tuple ts ->
              let loc = vd.pval_loc in
              List.fold_right ts ~init:[%type: goal] ~f:(fun t ->
                  ptyp_arrow ~loc Nolabel [%type: (asdf, qwerty) injected])
            | _ -> failwith "not yet implemented"
          in
          { ss with psig_desc = Psig_value { vd with pval_type = new_typ } })
      | _ -> ss
  end
;;

let () =
  (* Format.printf "Registering `%s`\n%!" name; *)
  Ppxlib.Driver.register_transformation ~intf:obj#signature "noinjected"
;;
