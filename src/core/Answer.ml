type t = Env.t * Term.t

let make env t = (env, t)

let env (env, _) = env

let unctr_term (_, t) =
  Term.map t
    ~fval:(fun x -> Term.repr x)
    ~fvar:(fun v -> Term.repr {v with Var.constraints = []})

let ctr_term (_, t) = t

let disequality (env, t) =
  let rec helper acc x =
    Term.fold x ~init:acc
      ~fval:(fun acc _ -> acc)
      ~fvar:(fun acc var ->
        ListLabels.fold_left var.Var.constraints ~init:acc
          ~f:(fun acc ctr_term ->
            let ctr_term = Term.repr ctr_term in
            let var = {var with Var.constraints = []} in
            let term = unctr_term @@ (env, ctr_term) in
            let acc = Binding.({var; term})::acc in
            helper acc ctr_term
          )
      )
  in
  helper [] t

let lift env' (env, t) =
  let vartbl = VarTbl.create 31 in
  let rec helper x =
    Term.map x
      ~fval:(fun x -> Term.repr x)
      ~fvar:(fun v -> Term.repr @@
        try
          VarTbl.find vartbl v
        with Not_found ->
          let new_var = Env.fresh ~scope:Var.non_local_scope env' in
          VarTbl.add vartbl v new_var;
          {new_var with Var.constraints =
            List.map (fun x -> helper x) v.Var.constraints
            |> List.sort Term.compare
          }
      )
  in
  (env', helper t)

let check_envs_exn env env' =
  if Env.equal env env' then () else
    failwith "OCanren fatal (Answer.check_envs): answers from different environments"

let equal (env, t) (env', t') =
  check_envs_exn env env';
  Term.equal t t'

let hash (env, t) = Term.hash t