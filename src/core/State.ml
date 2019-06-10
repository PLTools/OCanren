type t =
  { env   : Env.t
  ; subst : Subst.t
  ; ctrs  : Disequality.t
  ; scope : Var.scope
  }

type reified = Env.t * Term.t

let empty () =
  { env   = Env.empty ()
  ; subst = Subst.empty
  ; ctrs  = Disequality.empty
  ; scope = Var.new_scope ()
  }

let env   {env} = env
let subst {subst} = subst
let constraints {ctrs} = ctrs
let scope {scope} = scope

let fresh {env; scope} = Env.fresh ~scope env

let new_scope st = {st with scope = Var.new_scope ()}

let unify x y ({env; subst; ctrs; scope} as st) =
    match Subst.unify ~scope env subst x y with
    | None -> None
    | Some (prefix, subst) ->
      match Disequality.recheck env subst ctrs prefix with
      | None      -> None
      | Some ctrs -> Some {st with subst; ctrs}

let diseq x y ({env; subst; ctrs; scope} as st) =
  match Disequality.add env subst ctrs x y with
  | None      -> None
  | Some ctrs -> Some {st with ctrs}

let reify x {env; subst; ctrs} =
  let answ = Subst.reify env subst x in
  let diseqs = Disequality.reify env subst ctrs x in
  if List.length diseqs = 0 then
    [Answer.make env answ]
  else
    ListLabels.map diseqs ~f:(fun diseq ->
      let rec helper forbidden t =
        Term.map t
          ~fval:(fun x -> Term.repr x)
          ~fvar:(fun v -> Term.repr @@
            if List.mem v.Var.index forbidden then v
            else
              {v with Var.constraints =
                Disequality.Answer.extract diseq v
                |> List.filter (fun dt ->
                  match Env.var env dt with
                  | Some u  -> not (List.mem u.Var.index forbidden)
                  | None    -> true
                )
                |> List.map (fun x -> helper (v.Var.index::forbidden) x)
                (* TODO: represent [Var.constraints] as [Set];
                 * TODO: hide all manipulations on [Var.t] inside [Var] module;
                 *)
                |> List.sort Term.compare
              }
          )
      in
      Answer.make env (helper [] answ)
  )