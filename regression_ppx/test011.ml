(** This test demostrated that we need type annotation near prj_exn/reify
to avoid compilation error about `_weak` type variables
  *)

let () = print_endline "test010"

module JType = struct
  [%%distrib
  type 'targ ground =
    | Array of 'targ ground
    | Var of GT.int
  [@@deriving gt ~options:{ fmt; gmap }]]
end

[%%distrib type targ = Typ of targ JType.ground [@@deriving gt ~options:{ fmt; gmap }]]
(* include struct
  type nonrec 'a0 t = Typ of 'a0 [@@deriving gt ~options:{ fmt; gmap }]
  type ground = ground JType.ground t [@@deriving gt ~options:{ fmt }]
  type logic = logic JType.logic t OCanren.logic [@@deriving gt ~options:{ fmt; gmap }]
  type injected = injected JType.injected t OCanren.ilogic

  let fmapt f__012_ subj__013_ =
    let open OCanren.Env.Monad in
    OCanren.Env.Monad.return (GT.gmap t) <*> f__012_ <*> subj__013_
  ;;

  (* let (_ : (int, int) OCanren.Reifier.t) = JType.prj_exn *)

  let prj_exn : (injected, ground) OCanren.Reifier.t =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt (JType.prj_exn self)))
  ;;

  let reify : (injected, logic) OCanren.Reifier.t =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
        OCanren.reify
        <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt (JType.reify self)))))
  ;;

  let typ _x__010_ = OCanren.inji (Typ _x__010_)
end *)
