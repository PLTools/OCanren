open Logic
open Term
open Format

module T = Aez.Smt.Term
module F = Aez.Smt.Formula
module Solver = Aez.Smt.Make (struct end)
module Symbol = Aez.Smt.Symbol
module Type = Aez.Smt.Type

let (!!!) = Obj.magic;;

@type var_idx = GT.int with fmt
@type term = Var of var_idx | Const of GT.int with fmt
@type item =
  | FMDom of var_idx * GT.int GT.list
  | FMLT of term * term
  | FMLE of term * term
  | FMEQ of term * term
  | FMNEQ of term * term
  with fmt

type inti = (int, int logic) injected
type ph_desc = item list
type t = (VarSet.t * ph_desc) list

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

let check_item_list is =
  (* Construct request to solver there and check that it is satisfiable.
  *)
  try
    Solver.clear ();
    let wrap_binop op a b = F.make_lit op [ wrap_term a; wrap_term b ] in
    let make op xs =
      match op,xs with
      | F.Or, [x] -> x
      | _ -> F.make op xs
    in
    is |> Stdlib.List.iteri (fun id -> function
      | FMLT (a,b) ->
(*          Format.printf "%s %d\n%!" __FILE__ __LINE__;*)
          Solver.assume ~id @@ wrap_binop F.Lt a b
      | FMLE (a,b) ->
(*          Format.printf "%s %d\n%!" __FILE__ __LINE__;*)
          Solver.assume ~id @@ wrap_binop F.Le a b
      | FMEQ (a,b) ->
(*          Format.printf "%s %d\n%!" __FILE__ __LINE__;*)
          Solver.assume ~id @@ wrap_binop F.Eq a b
      | FMNEQ(a,b) ->
(*          Format.printf "%s %d\n%!" __FILE__ __LINE__;*)
          Solver.assume ~id @@ wrap_binop F.Neq a b
      | (FMDom(v,xs)) as xxx ->
(*          Format.printf "%s %d %a\n%!" __FILE__ __LINE__ (GT.fmt item) xxx;*)
          Solver.assume ~id @@
          make F.Or (xs |> Stdlib.List.map (fun n ->
            wrap_binop F.Eq (Var v) (Const n)
          ))
    );

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
(*
type lookup_rez =
  | Two of (VarSet.t * ph_desc) * (VarSet.t * ph_desc)
  | One of (VarSet.t * ph_desc)
  | Zero

let rec fold_cps ~f ~init xs =
  match xs with
  | [] -> init
  | x::xs -> f init x (fun acc -> fold_cps ~f ~init:acc xs)
*)
(*
let lookup a b (set,is) =

  let extend v = function
  | Zero -> One v
  | One v1 -> Two (v1, v)
  | Two (_,_) -> failwith "should not happpen"
  in
  let flt v1 v2 xs =
    fold_cps ~init:Zero ~f:(fun acc x k ->
      match x with
      | (set, items) when Term.VarSet.mem v1 set ->
    )
  in
    List.fold_left (fun acc x ->
      match (acc,x) with
      | T
    ) Zero xs
  in

  let f x =
    match Term.is_var x with
    | None ->
  | Some v1, Some v2 -> begin
      let verdict =

      in
  end
*)


let add_binop op (a: inti) (b:inti) (set,is) : t option =
  let set, ta, tb =
    let f set x = match Term.var x with
      | None   -> (set, Const (Obj.magic x))
      | Some v -> (VarSet.add v set, Var v.Term.Var.index)
    in
    let set,ta = f set !!!a in
    let set,tb = f set !!!b in
    (set, ta, tb)
  in
  let is = (op ta tb)::is in
  (set, is)

let check (_,is) =
  if check_item_list is
  then Some (set, is)
  else None

let neq x y t = check @@ add_binop (fun a b -> FMEQ  (a,b)) x y t
let eq  x y t = check @@ add_binop (fun a b -> FMNEQ (a,b)) x y t
let lt  x y t = check @@ add_binop (fun a b -> FMLT  (a,b)) x y t
let (=/=) = neq

let domain v ints (set,is) =
  let (set,v) =
    match Term.var v with
    | None -> failwith "should not happen"
    | Some v  -> (VarSet.add v set, v)
  in
  let d = FMDom (v.Term.Var.index, ints) in
  let is = d::is in
  if check_item_list is
  then Some (set, is)
  else None



let rec fold_cps ~f ~init xs =
  match xs with
  | [] -> init
  | x::xs -> f init x xs (fun acc -> fold_cps ~f ~init:acc xs)

let empty () = []

exception Bad
type lookup_rez =
  | One of VarSet.t * ph_desc
  | Zero

let recheck _env _subst t _prefix =
  let open Subst in
  (* We should iter prefix and see if some new substitution affect our constraints.
     In some cases our constraints can be merged
  *)

  let on_var_and_term v term store =
    try
      let ans =
        fold_cps ~init:[] store (fun acc (set,is) tl k ->
          if VarSet.mem v set
          then
            let st = add_binop (fun a b -> FMEQ (a,b)) !!!v !!!term in
            match check st with
            | None -> raise Bad
            | Some -> acc @ (set,st) :: tl
        )
      in
      (* If something bad happends on the way -- exception *)
      Some ans
    with Bad -> None
  in

  let on_two_vars v1 v2 store =
    let ans =
      fold_cps ~init:(Zero,[]) store (fun acc (set,is) tl k ->
        if VarSet.mem v set
        then
      )
    in
    match ans with
    | (Zero,_) ->
        let s = VarSet.(add v1 (add v2 empty)) in
        add_binop (fun a b -> FMEQ (a,b)) !!!v1 !!!v2 (empty ())
    | One (set,is),tl ->

  in

  Stdlib.List.fold_left (fun acc bin ->
    (* TODO: optimize with early exit *)
    match acc with
    | None -> None
    | Some (set, t) when VarSet.mem bin.Subst.Binding.var set ->
        eq !!!(bin.Binding.var) !!!(bin.Binding.term) (set,t)
    | _ -> acc
  ) (Some t) _prefix

let check ((set,is) as t) =
  if check_item_list is
  then Some t
  else None

