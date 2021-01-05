open Logic
open Term
open Format


let rec fold_cps ~f ~init xs =
  match xs with
  | [] -> init
  | x::xs -> f init x xs (fun acc -> fold_cps ~f ~init:acc xs)

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
  type pack

  val empty: unit -> t
  val is_var_interesting: t -> Term.Var.t -> bool

  val add_binop : (term -> term -> phormula) ->
           inti -> inti -> pack -> pack

  val check: t -> t option

  (* val check_item_list: phormula list -> bool *)
  val on_two_vars: (term -> term -> phormula) ->
           VarTbl.key ->
           VarTbl.key -> t -> t option

  val recheck_helper1: (term -> term -> phormula) -> t -> 'a -> 'b -> t option

  val add_domain: Term.Var.t -> int list -> t -> t option
end = struct
  type pack = VarSet.t * phormula list
  type nonrec t = pack list

  type lookup_rez =
    | One of pack
    | Zero


  let empty () = []

  let is_var_interesting _ _ = true
  exception Bad

  let add_binop op (a: inti) (b:inti) (set,is) : pack =
    let set, ta, tb =
      let f set x = match Term.var x with
        | None   -> (set, Const (Obj.magic x))
        | Some v -> (VarSet.add v set, Var v.Term.Var.index)
      in
      let set,ta = f set !!!a in
      let set,tb = f set !!!b in
      (set, ta, tb)
    in
    let is = (op ta tb) :: is in
    (set, is)

  let wrap_binop op a b = F.make_lit op [ wrap_term a; wrap_term b ]
  let make op xs =
    match op,xs with
    | F.Or, [x] -> x
    | _ -> F.make op xs

  let check_item_exn id = function
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

  let check_pack: pack -> bool = fun (_,is) ->
    (* Construct request to solver there and check that it is satisfiable.
    *)
    try
      Solver.clear ();
      Stdlib.List.iteri check_item_exn is;

      if is <> [] then Solver.check ();
      true
    with
      | Aez.Smt.Unsat _core -> false
      | Aez.Smt.Error e ->
          let open Aez in
          match e with
          | Smt.UnknownSymb s ->
              failwith (Printf.sprintf "unknown symb %s on %s %d\n%!" (Hstring.view s) __FILE__ __LINE__)
          | DuplicateSymb s ->
              failwith (Printf.sprintf "DuplicateSymb %s\n%!" (Aez.Hstring.view s))
          | DuplicateTypeName s -> failwith (Printf.sprintf "DuplicateTypeName %s\n%!" (Hstring.view s))
          | UnknownType s -> failwith (Printf.sprintf "unknown type '%s'. \n%!" (Hstring.view s))


  (* let check_item i = check_item_list [i] *)

  (* let check store =
    if check_item_list @@ snd store
    then Some store
    else None *)

  let check store : t option =
    try
      store |> Stdlib.List.iter (fun p ->
        if not (check_pack p) then raise Bad
      );
      Some store
    with Bad -> None

  let on_two_vars op v1 v2 store =
    let ext_set set = VarSet.(add v1 (add v2 set)) in
    let ans =
      fold_cps ~init:(Zero,[]) store ~f:(fun acc ((set,is) as a) tl k ->
        if VarSet.mem v1 set || VarSet.mem v2 set
        then begin
            match acc with
            | Zero,tl -> k (One a, tl)
            | One (s2,is2),tl2 ->
                let s3 = VarSet.(add v1 (add v2 (union s2 set))) in
                let is3 = Stdlib.List.append is is2 in
                (One (s3,is3), Stdlib.List.append tl2 tl)
        end else
          let v,xs = acc in
          k (v,a::xs)
      )
    in
    let (set,is,tl) =
      match ans with
      | Zero, tl -> (VarSet.empty, [], tl)
      | One (s,is),tl -> (s,is,tl)
    in

    (* we need to prepend to tail new pack of constraints *)
    let set = ext_set set in
    let h = add_binop op !!!v1 !!!v2 (set,is) in
    match check_pack h with
      | true -> Some (h::tl)
      | false -> None


  let recheck_helper1 op (store: t) a b =
    let open Subst in
    (* We should iter prefix and see if some new substitution affect our constraints.
      In some cases our constraints can be merged
    *)

    let on_var_and_term v term store =
      (* printf "on_var_and_term %s %d\n" __FILE__ __LINE__; *)
      try
        let ans =
          fold_cps ~init:[] store ~f:(fun acc ((set,is) as pack) tl k ->
            if VarSet.mem v set
            then
              let st = add_binop op !!!v !!!term (set, is) in
              match check_pack st with
              | false -> raise Bad
              | true  -> acc @ pack :: tl
            else
              k ( (set,is) :: acc)
          )
        in
        (* If something bad happends on the way -- exception *)
        Some ans
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
    let d = FMDom (v.Term.Var.index, ints) in
    if is_var_interesting v.Term.Var.index store
    then
      try
        fold_cps ~init:[] store ~f:(fun acc (set,phs) tl k ->
          if VarSet.mem v set
          then begin
            let phs = d::phs in
            if check_pack (set,phs)
            then (set, phs) :: (acc @ tl)
            else raise Bad
          end else
            k ((set, phs) :: acc)
        ) |> (fun x -> Some x)
      with Bad -> None
    else
      Some (VarSet.(add v empty, [d]) :: store)


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
