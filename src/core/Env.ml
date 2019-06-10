type t = {anchor : Var.env; mutable next : int}

let last_anchor = ref 11
let first_var = 10

let empty () =
  incr last_anchor;
  {anchor = !last_anchor; next = first_var}

let create ~anchor = {anchor; next = first_var}

let fresh ~scope e =
  let v = !!!(Var.make ~env:e.anchor ~scope e.next) in
  e.next <- 1 + e.next;
  !!!v

let check env v = (v.Var.env = env.anchor)

let check_exn env v =
  if check env v then () else failwith "OCanren fatal (Env.check): wrong environment"

let var env x =
  match Term.var x with
  | (Some v) as res -> check_exn env v; res
  | None            -> None

let is_var env x = (var env x) <> None

let freevars env x =
  Term.fold (Term.repr x) ~init:VarSet.empty
    ~fvar:(fun acc v -> VarSet.add v acc)
    ~fval:(fun acc _ -> acc)

exception Open_Term

let is_open env x =
  try
    Term.iter (Term.repr x)
      ~fvar:(fun _ -> raise Open_Term)
      ~fval:(fun _ -> ());
    false
  with Open_Term -> true

let equal {anchor=a1} {anchor=a2} = (a1 = a2)