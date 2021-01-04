(*
 * pa_ocanren: a camlp5 extension to implement syntax-level
 * miniKanren constructs.
 * Copyright (C) 2015-2020
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

let decapitalize s =
  String.init (String.length s) (function 0 -> Char.lowercase_ascii s.[0] | i -> s.[i])

let rec ctor e =
  let loc = MLast.loc_of_expr e in
  match e with
  | <:expr< $uid:u$ >>   -> Some (<:expr< $lid:decapitalize u$ >>)
  | <:expr< $m$ . ($e$) >> -> (match ctor e with Some e -> Some (<:expr< $m$ . ($e$) >>) | _ -> None)
  | _                    -> None

let list_of_list es =
  let loc      = MLast.loc_of_expr (List.hd es) in
  let cons a b = <:expr< [ $a$ :: $b$ ]  >> in
  List.fold_right (fun e lst -> cons e lst) es <:expr< [] >>

let rec fix_term e =
  let loc = MLast.loc_of_expr e in
  match e with
  | <:expr< $e1$ $e2$ >> ->
     (match ctor e1 with
      | Some e1' ->
         (match e2 with
          | <:expr< ( $list:ts$ ) >> ->
             List.fold_left (fun acc e -> <:expr< $acc$ $fix_term e$ >> ) e1' ts
          | _  -> <:expr< $e1'$ $fix_term e2$ >>
         )
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
     | Some e -> <:expr<$e$ () >>
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

let of_val (Ploc.VaVal x) = x

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




EXTEND
  GLOBAL: expr ctyp str_item;

  long_ident:
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
    ]];

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
      let ans =
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
      in
      ans
    ] |
    [ "defer"; subj=expr LEVEL "." ->
      <:expr< delay (fun () -> $subj$) >>
    ] |
    [ e=ocanren_embedding -> e ]
  ];

  ocanren_embedding: [
    [ "ocanren"; "{"; e=ocanren_expr; "}" -> e ]
  ];

  ocanren_expr: [
    "top" RIGHTA [ l=SELF; "|"; r=SELF -> <:expr< OCanren.disj $l$ $r$ >> ] |
          RIGHTA [ l=SELF; "&"; r=SELF -> <:expr< OCanren.conj $l$ $r$ >> ] |
    [ "fresh"; vars=LIST1 LIDENT SEP ","; "in"; b=ocanren_expr LEVEL "top" ->
       List.fold_right
         (fun x b ->
            let p = <:patt< $lid:x$ >> in
            <:expr< OCanren.call_fresh ( fun $p$ -> $b$ ) >>
         )
         vars
         b
    ] |
    "primary" [
        p=prefix; t=ocanren_term                      -> let p = <:expr< $lid:p$ >> in <:expr< $p$ $t$ >>
      | l=ocanren_term; "==" ; r=ocanren_term         -> <:expr< OCanren.unify $l$ $r$ >>
      | l=ocanren_term; "=/="; r=ocanren_term         -> <:expr< OCanren.diseq $l$ $r$ >>
      | l=ocanren_term; op=operator; r=ocanren_term   -> let p = <:expr< $lid:op$ >> in
                                                         let a = <:expr< $p$ $l$ >> in
                                                         <:expr< $a$ $r$ >>
      | x=ocanren_term                                -> x
      | "{"; e=ocanren_expr; "}"                      -> e
      | "||"; "("; es=LIST1 ocanren_expr SEP ";"; ")" -> <:expr< OCanren.conde $list_of_list es$ >>
      | "&&"; "("; es=LIST1 ocanren_expr SEP ";"; ")" ->
         let op = <:expr< $lid:"?&"$ >> in
         let id = <:expr< OCanren . ($op$) >> in
         <:expr< $id$ $list_of_list es$ >>
    ]
  ];

  ocanren_term: [[
    t=ocanren_term' -> fix_term t
  ]];

  ocanren_term':  [
    "app"  LEFTA  [ l=SELF; r=SELF -> <:expr< $l$ $r$ >>] |
    "list" RIGHTA [ l=SELF; "::"; r=SELF -> <:expr< OCanren.Std.List.cons $l$ $r$ >> ] |
    "primary" [ "!"; "("; e=expr; ")" -> e
    | c=INT ->
      let n = <:expr< $int:c$ >> in
      <:expr< OCanren.Std.nat $n$ >>
    | c=CHAR ->
       let s = <:expr< $chr:c$ >> in
       <:expr< OCanren.inj (OCanren.lift $s$) >>
    | s=STRING ->
      let s = <:expr< $str:s$ >> in
      <:expr< OCanren.inj (OCanren.lift $s$) >>
    | "true"   -> <:expr< OCanren.Std.Bool.truo >>
    | "false"  -> <:expr< OCanren.Std.Bool.falso >>
    | "["; ts=LIST0 ocanren_term' SEP ";"; "]" ->
      (match ts with
       | [] -> <:expr< OCanren.Std.nil () >>
       | _  -> List.fold_right (fun x l -> <:expr< OCanren.Std.List.cons $x$ $l$ >> ) ts <:expr< OCanren.Std.nil () >>
      )
    | "("; op=operator_rparen                  -> <:expr< $lid:op$ >>
    | "("; ts=LIST0 ocanren_term' SEP ","; ")" ->
      (match ts with
       | []  -> <:expr< OCanren.inj (OCanren.lift ()) >>
       | [t] -> t
       | _   -> <:expr< ( $list:ts$ ) >>
      )
    ] |
    [ long_ident ]
  ];

  ctyp: [
             [ "ocanren"; "{"; t=ctyp; "}" -> decorate_type t ] |
    "simple" [ "!"; "("; t=ctyp; ")" -> <:ctyp< ocanren $t$ >> ]
  ];

END;
