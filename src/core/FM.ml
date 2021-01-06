open Logic
open Term
open Format


let rec fold_cps ~f ~init xs =
  match xs with
  | [] -> init
  | x::xs -> f init x xs (fun acc -> fold_cps ~f ~init:acc xs)


let list_fold_lefti ~init ~f =
  let rec helper acc i = function
  | [] -> acc
  | x::xs -> helper (f acc i x) (i+1) xs
  in
  helper init 0

module T = Aez.Smt.Term
module F = Aez.Smt.Formula
module Solver = Aez.Smt.Make (struct end)
module Symbol = Aez.Smt.Symbol
module Type = Aez.Smt.Type

let (!!!) = Obj.magic;;

@type var_idx = GT.int with fmt
@type term = Var of var_idx | Const of GT.int with fmt
@type phormula =
  | FMDom of var_idx * GT.int GT.list
  | FMLT of term * term
  | FMLE of term * term
  | FMEQ of term * term
  | FMNEQ of term * term
  with fmt

type inti = (int, int logic) injected
(* type ph_desc = phormula list *)
(* type item = VarSet.t * ph_desc *)



let var_of_idx idx = Aez.Hstring.make (sprintf "x%d" idx)
let decl_var idx =
  let x = var_of_idx idx in
  try
    Symbol.declare x [] Type.type_int
  with
    | Aez.Smt.Error (Aez.Smt.DuplicateSymb _) -> ()
    | Aez.Smt.Error (Aez.Smt.DuplicateTypeName _) -> ()

let wrap_term = function
  | Var n   ->
      decl_var n;
      T.make_app (var_of_idx n) []
  | Const m -> T.make_int (Num.num_of_int m)


module Store : sig
  type t

  val empty: unit -> t
  val is_var_interesting: t -> Term.Var.t -> bool

  val check: t -> t option

  val recheck_helper1: (term -> term -> phormula) -> t -> 'a -> 'b -> t option

  val add_domain: Term.Var.t -> int list -> t -> t option
end = struct

  let wrap_binop op a b = F.make_lit op [ wrap_term a; wrap_term b ]
  let make op xs =
    match op,xs with
    | F.Or, [x] -> x
    | _ -> F.make op xs

  let assume_item_exn id = function
    | FMLT (a,b) ->
        Solver.assume ~id @@ wrap_binop F.Lt a b
    | FMLE (a,b) ->
        Solver.assume ~id @@ wrap_binop F.Le a b
    | FMEQ (a,b) ->
        Solver.assume ~id @@ wrap_binop F.Eq a b
    | FMNEQ(a,b) ->
        Solver.assume ~id @@ wrap_binop F.Neq a b
    | (FMDom(v,xs)) ->
        Solver.assume ~id @@
        make F.Or (xs |> Stdlib.List.map (fun n ->
          wrap_binop F.Eq (Var v) (Const n)
        ))

  module Pack = struct
    type t = { vars: VarSet.t; mutable state: Solver.state; phs: phormula list }

    let size : t -> int = fun {phs} -> Stdlib.List.length phs
    let state {state} = state

    let make vars state phs = { vars; state; phs }
    let empty () =
      Solver.clear ();
      make VarSet.empty (Solver.save_state ()) []

    let pp fmt pack =
      Format.pp_print_list (GT.fmt phormula) Format.std_formatter pack.phs

    (* TODO: add phantom argument checked/nonchecked *)
    let singleton : _ -> Term.Var.t -> 'a -> t = fun op var t ->
      let vars, ta, tb =
        let f set x = match Term.var x with
          | None   -> (set, Const (Obj.magic x))
          | Some v -> (VarSet.add v set, Var v.Term.Var.index)
        in
        let set = VarSet.empty in
        let set,ta = f set !!!var in
        let set,tb = f set !!!t in
        (set, ta, tb)
      in
      Solver.clear ();
      let state = Solver.save_state () in
      make vars state [op ta tb]

    let domain v ints =
      let set =
        match Term.var v with
        | Some v -> VarSet.singleton v
        | None -> VarSet.empty
      in
      { empty () with vars = set; phs = [ FMDom (v.Term.Var.index, ints) ] }

    let refresh ({phs} as pack) =
      Solver.clear ();
      Stdlib.List.iteri assume_item_exn phs;
      pack.state <- Solver.save_state ()


  end

  type t = Pack.t list

  type lookup_rez =
    | One of Pack.t
    | Zero


  let empty () = []

  let is_var_interesting _ _ = true
  exception Bad



  let check_pack : Pack.t -> bool = function { state } as p ->
    try
      printf "check_pack %s %d the pack of size %d\n" __FILE__ __LINE__ (Pack.size p);
      Format.printf "%a\n%!" Pack.pp p ;
      Pack.refresh p;
      Solver.clear ();
      Solver.restore_state state;
      Solver.check ();
      true
  with Aez.Smt.Unsat _core -> false


  let rec merge_packs p1 p2  =
    if Pack.(size p1 > size p2) then merge_packs p2 p1
    else
      let () = Solver.restore_state (Pack.state p2) in
      let p2_size = Pack.size p2 in
      let ph3 =
        list_fold_lefti p1.Pack.phs ~init:p2.Pack.phs ~f:(fun acc i ph ->
          assume_item_exn (i+p2_size) ph;
          (ph::acc)
        )
      in
      let set3 = VarSet.union p2.Pack.vars p1.Pack.vars in
      Pack.make set3 (Solver.save_state ()) ph3


  let check store : t option =
    try
      store |> Stdlib.List.iter (fun p ->
        if not (check_pack p) then raise Bad
      );
      Some store
    with Bad -> None

  let on_two_vars op v1 v2 store =
    (* let ext_set set = VarSet.(add v1 (add v2 set)) in *)
    let ans =
      fold_cps ~init:(Zero,[]) store ~f:(fun acc pack tl k ->
        if VarSet.mem v1 pack.Pack.vars || VarSet.mem v2 pack.Pack.vars
        then begin
            match acc with
            | Zero,tl -> k (One pack, tl)
            | (One pack2),tl2 ->
                let pack3 = merge_packs pack pack2 in
                (One pack3, Stdlib.List.append tl2 tl)
        end else
          let v,xs = acc in
          k (v,pack::xs)
      )
    in
    let (p, other_packs) =
      match ans with
      | Zero, tl -> (Pack.empty (), tl)
      | One pack,tl -> (pack,tl)
    in

    (* we need to prepend to tail new pack of constraints *)
    let p_new = Pack.singleton op v1 v2 in
      (* let state =
        Solver.clear ();
        Solver.save_state ()
      in
      let (set,h) = add_binop op !!!v1 !!!v2 (VarSet.empty, []) in
      Pack.make set state h
    in *)

    let pack = merge_packs p_new p in
    match check_pack pack with
      | true -> Some (pack::other_packs)
      | false -> None


  let recheck_helper1 op (store: t) a b =
    let open Subst in
    (* We should iter prefix and see if some new substitution affect our constraints.
      In some cases our constraints can be merged
    *)

    let on_var_and_term v term store =
      (* printf "on_var_and_term %s %d\n" __FILE__ __LINE__; *)
      try
        fold_cps ~init:[] store ~f:(fun acc pack tl k ->
          if VarSet.mem v pack.Pack.vars
          then
            let new_pack = Pack.singleton op v term in
            let p = merge_packs new_pack pack in
            match check_pack p with
            | false -> raise Bad
            | true  -> acc @ pack :: tl
          else
            k ( pack :: acc)
        ) |> Stdlib.Option.some
      with Bad -> None
    in


    (* let () = printf "a  = %s\n" (Term.show !!!a) in
    let () = printf "b  = %s\n" (Term.show !!!b) in *)
    match Term.(var a, var b) with
    | None,None when !!!a = !!!b -> Some store
    | None,None                  -> None
    | Some v1, Some v2 -> on_two_vars op v1 v2 store
    | Some v, x
    | x, Some v -> on_var_and_term v x store


  let add_domain v ints store =
    (* if is_var_interesting v.Term.Var.index store
    then *)
      try
        fold_cps ~init:[] store ~f:(fun acc pack tl k ->
          if VarSet.mem v pack.Pack.vars
          then begin
            let new_pack = Pack.domain v ints in
            let p = merge_packs new_pack pack in
            match check_pack p with
            | false -> raise Bad
            | true  -> acc @ pack :: tl
          end else
            k (pack :: acc)
        ) |> Stdlib.Option.some
      with Bad -> None
    (* else
      Some (VarSet.(add v empty, [d]) :: store)
 *)

end


type t = Store.t

exception Bad
let empty () = Store.empty ()

let recheck_helper op (store: Store.t) (_prefix : Subst.Binding.t list) =
  try
    Some (ListLabels.fold_left ~init:store _prefix ~f:(fun acc bin ->
(*
      let acc =
        match Term.var bin.Binding.term with
        | Some v2 -> on_two_vars bin.Binding.var v2 acc
        | None -> on_var_and_term bin.Binding.var !!!(bin.Binding.term) acc
      in*)

      if Store.is_var_interesting acc  Subst.(bin.Binding.var) then
        let open Subst in
        match Store.recheck_helper1 op acc !!!(bin.Binding.var) !!!(bin.Binding.term) with
        | None -> raise Bad
        | Some ans -> ans
      else acc
    ))
  with Bad -> None

let recheck _env _subst (store: Store.t) (_prefix : Subst.Binding.t list) =
  (* printf "%s %d length of _prefix=%d\n" __FILE__ __LINE__ (Stdlib.List.length _prefix); *)
  match recheck_helper (fun a b -> FMEQ (a,b)) store _prefix with
  | None ->
     (* printf "recheck failed\n";  *)
     None
  | x -> x

let check = Store.check




let neq x y store =
  Store.recheck_helper1 (fun a b -> FMNEQ (a,b)) store x y

let eq  x y store =
  Store.recheck_helper1 (fun a b -> FMEQ (a,b)) store x y

let lt  x y store =
  Store.recheck_helper1 (fun a b -> FMLT (a,b)) store x y

let (=/=) = neq

let domain (v: inti) ints store =
  let v =
    match Term.var !!!v with
    | None -> failwith "should not happen"
    | Some v  -> v
  in

  Store.add_domain v ints store
(*
  try
    fold_cps ~init:[] store ~f:(fun acc (set,is) tl k ->
      if VarSet.mem v set
      then begin
        let d = FMDom (v.Term.Var.index, ints) in
        let is = d::is in
        if Store.check_item_list is
        then (VarSet.add v set, is) :: (acc @ tl)
        else raise Bad
      end else
        k ((set,is)::acc)
    ) |> (fun x -> Some x)
  with Bad -> None *)
