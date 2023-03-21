(*
 * pa_ocanren: a camlp5 extension to implement syntax-level
 * miniKanren constructs.
 * Copyright (C) 2015-2023
 * Dmitri Boulytchev, St.Petersburg State University
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


(** {1 Camlp5 syntax extension for miniKanren syntax constructs}

  There are two syntax extensions provided: [fresh] and [defer].

  [fresh] is a direct analog to the corresponding construct in original miniKanren. It has the form
  [fresh (x y ...) g], where [x y ...] is a list of free variables, created by [fresh], [g] --- some goal.

  [defer] performs "inverse-eta-delay". It has the form [defer (g)] and expanded into [fun st -> Lazy.from_fun (fun () -> g st)].
*)

open Pcaml
open Printf

let rec fold_right1 f = function
| [h]  -> h
| h::t -> f h (fold_right1 f t)
| []   -> failwith "fold_right1"

let rec fold_left1 f xs = List.fold_left f (List.hd xs) (List.tl xs)

let decapitalize =
  String.mapi (function 0 -> Char.lowercase_ascii | _ -> Fun.id)

let rec ctor e =
  let loc = MLast.loc_of_expr e in
  match e with
  | <:expr< $uid:u$ >>            -> Some e
  | <:expr< $longid:m$ . ($e$) >> -> (match ctor e with Some e -> Some (<:expr< $longid:m$ . ($e$) >>) | _ -> None)
  | <:expr< $m$ . ($e$) >>        -> (match ctor e with Some e -> Some (<:expr< $m$ . ($e$) >>) | _ -> None)
  | _                             -> None

let list_of_list es =
  let loc      = MLast.loc_of_expr (List.hd es) in
  let cons a b = <:expr< [ $a$ :: $b$ ]  >> in
  List.fold_right (fun e lst -> cons e lst) es <:expr< [] >>

let gensym =
  let ans = ref 0 in
  fun ?(prefix="") () ->
    incr ans;
    Format.asprintf "_%s_%d" prefix !ans

let pp_ast ppf x =
  Format.fprintf ppf "%s" (Eprinter.apply pr_expr Pprintf.empty_pc x)

let rec fix_term e =
  let loc = MLast.loc_of_expr e in
  match e with
  | <:expr< $e1$ $e2$ >> ->
     (match ctor e1 with
      | Some e1' ->
         let fixed =
           match e2 with
           | <:expr< ( $list:ts$ ) >> ->
              List.fold_left (fun acc e -> <:expr< $acc$ $fix_term e$ >> ) e1' ts
           | _  ->
              <:expr< $e1'$ $fix_term e2$ >>
         in
         <:expr< OCanren.inji $fixed$ >>

      | _ ->
         (match e with
          | <:expr< OCanren.Std.nil () >> -> e
          | _ -> <:expr< $fix_term e1$ $fix_term e2$ >>
         )
     )
  | <:expr< ( $list:ts$ ) >> ->
     (* isolated tuple case (not an argument to a constructor *)
     (match ts with
      | [e] -> fix_term e
      | _   -> fold_right1 (fun e tup -> <:expr< OCanren.Std.pair $e$ $tup$ >> ) @@ List.map fix_term ts
     )
  | _ ->
    (* everything else *)
    (match ctor e with
     | Some e -> <:expr< OCanren.inji $e$ >>
     | _ -> e
    )

(* Borrowed from camlp5 OCaml parser *)
let is_operator =
  let ht = Hashtbl.create 73 in
  let ct = Hashtbl.create 73 in
  List.iter (fun x -> Hashtbl.add ht x true)
    ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"];
  List.iter (fun x -> Hashtbl.add ct x true)
    ['!'; '*'; '+'; '-'; '/'; ':'; '<'; '='; '>'; '@'; '^'; '~';
     '?'; '%'; '.'; '$'];
  fun x ->
    try Hashtbl.find ht x with
    Not_found -> try Hashtbl.find ct x.[0] with _ -> false

let operator_rparen =
  Grammar.Entry.of_parser gram "operator_rparen"
    (fun strm ->
       match Stream.npeek 2 strm with
       | [("", s); ("", ")")] when is_operator s ->
           Stream.junk strm;
           Stream.junk strm;
           s
       | _ -> raise Stream.Failure)

let operator =
  Grammar.Entry.of_parser gram "operator"
    (fun strm ->
       match Stream.npeek 1 strm with
       | [("", s)] when is_operator s ->
           Stream.junk strm;
           s
       | _ -> raise Stream.Failure)

let symbolchar =
  let list =
    ['!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
     '@'; '^'; '|'; '~']
  in
  let rec loop s i =
    if i == String.length s then true
    else if List.mem s.[i] list then loop s (i + 1)
    else false
  in
  loop

let prefix =
  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"; "?!"] in
  Grammar.Entry.of_parser gram "prefixop"
    (fun strm ->
      match Stream.npeek 1 strm with
      | [("", s)] when not (List.mem s excl) && String.length s >= 2 &&
                            List.mem s.[0] list && symbolchar s 1 -> Stream.junk strm; s
      | _ -> raise Stream.Failure
    )

let op_from_list l =
  let b = Buffer.create 64 in
  let add = Buffer.add_string b in
  List.iter add l;
  Buffer.contents b

let of_val = function
  | Ploc.VaVal x -> x
  | Ploc.VaAnt _ -> failwith "Should not happen in our setup of Camlp5"

(* Convert to a logic type *)
let type_reifier mode ctyp =
  let loc = MLast.loc_of_ctyp ctyp in
  let m   =
    match mode with
    | `Project -> (fun name -> "prj_exn_" ^ name)
    | `Reify   -> (fun name -> "reify_" ^ name)
  in
  match ctyp with
  | <:ctyp< $lid:id$ >>             -> <:expr< $lid:(m id)$ >>
  | <:ctyp< $longid:p$ . $lid:t$ >> -> <:expr< $longid:p$ . $lid:(m t)$ >>
  | _                               -> raise (Stream.Error "INTERNAL ERROR: \"^\" expects a (qualified) type name")
     
(* Decorate type expressions *)
let rec decorate_type ctyp =
  let loc = MLast.loc_of_ctyp ctyp in
  match ctyp with
  | <:ctyp< int >>           -> <:ctyp< OCanren.Std.Nat.logic >>
  | <:ctyp< bool >>          -> <:ctyp< OCanren.Std.Bool.logic >>
  | <:ctyp< $lid:id$ >>      -> <:ctyp< OCanren.logic $ctyp$ >>
  | <:ctyp< ocanren  $t$ >>  -> t
  | <:ctyp< list $y$ >>      -> <:ctyp< OCanren.Std.List.logic $decorate_type y$ >>
  | <:ctyp< option $y$ >>    -> <:ctyp< OCanren.Std.Option.logic $decorate_type y$ >>
  | <:ctyp< $x$ $y$ >>       -> let t = <:ctyp< $x$ $decorate_type y$ >> in <:ctyp< OCanren.logic $t$ >>
  | <:ctyp< $longid:p$ . $lid:t$ >>  -> <:ctyp< OCanren.logic $ctyp$ >>
  | <:ctyp< ( $list:ts$ ) >> -> fold_right1 (fun t1 t2 -> <:ctyp< OCanren.Std.Pair.logic $t1$ $t2$ >> ) @@ List.map decorate_type ts
  | _                        -> ctyp

let add_freshes ~loc vars body =
  let rec loop = function
  | a::b::c::tl ->
      let pa = <:patt< $lid:a$ >> in
      let pb = <:patt< $lid:b$ >> in
      let pc = <:patt< $lid:c$ >> in
      <:expr< OCanren.Fresh.three (fun $pa$ $pb$ $pc$ -> $loop tl$) >>
  | a::b::tl ->
      let rez = loop tl in
      let pa = <:patt< $lid:a$ >> in
      let pb = <:patt< $lid:b$ >> in
      <:expr< OCanren.Fresh.two (fun $pa$ $pb$ -> $rez$) >>
  | a::[] ->
      let pa = <:patt< $lid:a$ >> in
      <:expr< OCanren.Fresh.one (fun $pa$ -> $body$) >>
  | []    -> body
  in
  loop vars

open Writer

let stream_peek_nth n (strm : (string * string) Stream.t) =
 let rec loop n =
    function
      []     -> None
    | [x]    -> if n = 1 then Some (x : (string * string)) else None
    | _ :: l -> loop (n - 1) l
 in
 loop n (Stream.npeek n strm)

let check_type_decl strm =
  match stream_peek_nth 1 strm with
    None -> assert false
  | Some (_, "type") -> true
  | _ -> false

let is_type_decl_f strm =
  if check_type_decl strm then ()
  else raise Stream.Failure

let is_type_decl = Grammar.Entry.of_parser gram "is_type_decl" is_type_decl_f

let decorate_type_decl t =
  let loc = MLast.loc_of_str_item t in
  let wrap ltd =
    List.map
      (function
      | <:type_decl< $tp:ls$ $list:ltv$ = 'abstract >> ->
          raise (Stream.Error "INTERNAL ERROR: abstract type declarations not implemented")
      | <:type_decl:< $tp:ls$ $list:ltv$ = $priv:b$ $t$ $_list:ltt$ $itemattrs:attrs$ >> ->
          let a =
            let loc, _ = ls in
            let gt = <:expr< gt ~{options = {gmap=gmap; show=show}} >> in
            <:attribute_body< deriving $exp:gt$; >>
          in
          let attrs = <:vala< a >> :: attrs in
          <:type_decl< $tp:ls$ $list:ltv$ = $priv:b$ $t$ $_list:ltt$ $itemattrs:attrs$ >>
      | _ -> raise (Stream.Error "INTERNAL ERROR: unsupported case, probably antiquotation")
      )
      ltd
  in
  match t with
  | <:str_item< type nonrec $list:ltd$ >> ->
      let ltd = wrap ltd in
      <:str_item< [%%ocanren_inject type nonrec $list:ltd$; ] >>
  | <:str_item< type $list:ltd$ >> ->
      let ltd = wrap ltd in
      <:str_item< [%%ocanren_inject type $list:ltd$; ] >>
  | _ -> raise (Stream.Error "INTERNAL ERROR: type_decl expected")

EXTEND
  GLOBAL: expr ctyp str_item;

  (* Kakadu: It looks like this function has become unneeded *)
  (* long_ident:
    [ RIGHTA
      [ i = LIDENT -> <:expr< $lid:i$ >>
      | i = UIDENT -> <:expr< $uid:i$ >>
      | "("; op=operator_rparen -> <:expr< $lid:op$ >>
      | i = UIDENT; "."; j = SELF ->
          let rec loop m =
            function
            | <:expr< $x$ . ($y$) >> -> loop <:expr< $m$ . ($x$) >> y
            | e                    -> <:expr< $m$ . ($e$) >>
          in
          loop <:expr< $uid:i$ >> j
    ]]; *)

  str_item: [
      ["ocanren"; is_type_decl; s=str_item -> decorate_type_decl s]
  ];

  (* TODO: support conde expansion here *)
  expr: LEVEL "expr1" [
    [ "fresh"; "("; vars=LIST0 LIDENT; ")"; clauses=LIST1 expr LEVEL "." ->
      let body =
        let conjunctions = fold_left1
          (fun acc x -> <:expr< conj ($acc$) ($x$) >>)
          clauses
        in
        <:expr< delay (fun () -> $conjunctions$) >>
      in
      add_freshes ~loc vars body
    ] |
    [ "defer"; subj=expr LEVEL "." ->
      <:expr< delay (fun () -> $subj$) >>
    ] |
    [ e=ocanren_embedding -> e ] |
    [ "ocanrun"; "("; args=ocanrun_args; ")"; "{"; goal=ocanren_expr; "}"; "->"; sema=expr ->
       let rec gen_numeral = function
         1 -> <:expr< OCanren.q   >>
       | 2 -> <:expr< OCanren.qr  >>
       | 3 -> <:expr< OCanren.qrs >>
       | 4 -> <:expr< OCanren.qrt >>
       | n -> <:expr< OCanren.succ $gen_numeral (n-1)$ >>
       in
       let fun_args, let_bnd =
         List.fold_left
           (fun (fun_args, let_bnd) (name, rei) ->
              let p = <:patt< $lid:name$  >> in
              let n = <:expr< $lid:name$  >> in
              let r = <:expr< $n$ # reify >> in
              (p :: fun_args, (p, <:expr< $r$ $rei$ >>, Ploc.VaVal []) :: let_bnd)
           )
           ([], [])
           args
       in
       let make_fun b =
         List.fold_left (fun expr arg -> <:expr< fun $arg$ -> $expr$ >> ) b fun_args
       in
       let fun_goal = make_fun goal in
       let let_body = <:expr< let $flag:false$ $list:let_bnd$ in $sema$ >> in
       let fun_sema = make_fun let_body in
       <:expr< OCanren.run $gen_numeral (List.length args)$ $fun_goal$ $fun_sema$ >>
    ]
  ];

  ocanrun_args: [[
    args=LIST1 ocanrun_arg SEP "," -> List.concat args 
  ]];

  ocanrun_arg: [[
    names=LIST1 LIDENT SEP ","; ":"; reiflag=OPT "^"; typ=ctyp ->
      let rei =
        match reiflag with
        | None -> type_reifier `Project typ
        | _    -> type_reifier `Reify   typ
      in
      List.map (fun name -> (name, rei)) names                                                     
  ]];
  
  ocanren_embedding: [
    [ "ocanren"; "{"; e=ocanren_expr; "}" -> e ]
  ];

  ocanren_expr: [
    "top" RIGHTA [ l=SELF; "|"; r=SELF -> <:expr< OCanren.disj $l$ $r$ >> ] |
          RIGHTA [ l=SELF; "&"; r=SELF -> <:expr< OCanren.conj $l$ $r$ >> ] |
    [ "fresh"; vars=LIST1 LIDENT SEP ","; "in"; b=ocanren_expr LEVEL "top" ->
        add_freshes ~loc vars b
    ] |
    "primary" [
        p=prefix; t=ocanren_term ->
          let p = <:expr< $lid:p$ >> in
          <:expr< $p$ $snd t$ >>
      | l=ocanren_term; "==" ; r=ocanren_term ->
          let (vars1, l) = l in
          let (vars2, r) = r in
          add_freshes ~loc (vars1@vars2) <:expr< OCanren.unify $l$ $r$ >>
      | l=ocanren_term; "=/="; r=ocanren_term         -> <:expr< OCanren.diseq $snd l$ $snd r$ >>
      | l=ocanren_term; op=operator; r=ocanren_term   ->
          let p = <:expr< $lid:op$ >> in
          let a = <:expr< $p$ $snd l$ >> in
          <:expr< $a$ $snd r$ >>
      | x=ocanren_term                                -> snd x
      | "{"; e=ocanren_expr; "}"                      -> e
      | "||"; "("; es=LIST1 ocanren_expr SEP ";"; ")" -> <:expr< OCanren.conde $list_of_list es$ >>
      | "&&"; "("; es=LIST1 ocanren_expr SEP ";"; ")" ->
          let op = <:expr< $lid:"?&"$ >> in
          let id = <:expr< OCanren . ($op$) >> in
          <:expr< $id$ $list_of_list es$ >>
    ]
  ];

  ocanren_term: [[ t=ocanren_term_m -> run (t >>| fix_term) ]];

  ocanren_term_m:
    [ "app"  LEFTA  [ l=SELF; r=SELF -> return (fun l r -> <:expr< $l$ $r$ >>) <*> l <*> r ]
    | "list" RIGHTA [ l=SELF; "::"; r=SELF ->
        return (fun l r -> <:expr< OCanren.Std.List.cons $l$ $r$ >>) <*> l <*> r
      ]
    | "primary"
        [ "!"; "("; e=expr; ")" -> return e
        | c=INT ->
            let n = <:expr< $int:c$ >> in
            return <:expr< OCanren.Std.nat $n$ >>
        | c=CHAR ->
            let s = <:expr< $chr:c$ >> in
            return <:expr< OCanren.inj (OCanren.lift $s$) >>
        | s=STRING ->
            let s = <:expr< $str:s$ >> in
            return <:expr< OCanren.inj (OCanren.lift $s$) >>
        | "true"   -> return <:expr< OCanren.Std.Bool.truo >>
        | "false"  -> return <:expr< OCanren.Std.Bool.falso >>
        | "_" ->
              let next = gensym () in
              write (return <:expr< $lid:next$ >>) next
        (* Double wildcard is completely optional *)
        (* | x=LIDENT -> (
            match x with
            | "__" ->
              let next = gensym () in
              write (return <:expr< $lid:next$ >>) next
            | _ -> return <:expr< $lid:x$ >>
        ) *)
        | "["; ts=LIST0 SELF SEP ";"; "]" ->
            (match ts with
            | [] -> return <:expr< OCanren.Std.nil () >>
            | _ ->
              List.fold_right (fun e acc -> return (fun a b -> <:expr< OCanren.Std.List.cons $a$ $b$ >>) <*> e <*> acc) ts
               (return <:expr< OCanren.Std.nil() >>)
            )
        | "("; op=operator_rparen         -> return <:expr< $lid:op$ >>
        | "("; ts=LIST0 SELF SEP ","; ")" ->
            (match ts with
            | []  -> return <:expr< () >>
            | [e] -> e
            | _   -> return (fun ts -> <:expr< ( $list:ts$ ) >>) <*>
                     List.fold_right (fun x acc -> return (fun x acc -> x :: acc) <*> x <*> acc) ts (return [])
            )
        ]
    | [ e = expr LEVEL "simple" -> return e ]
  ];
  
  ctyp: [
            [ "ocanren"; "{"; t=ctyp; "}" -> decorate_type t        ] | 
   "simple" [ "!"; "("; t=ctyp; ")"       -> <:ctyp< ocanren $t$ >> ] 
  ]; 

END;
