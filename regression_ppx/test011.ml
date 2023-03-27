(** This test demostrated that we need type annotation near prj_exn/reify
to avoid compilation error about `_weak` type variables
  *)

let () = print_endline "test010"

module JType = struct
  [%%distrib
  type 'targ ground =
    | Array of 'targ ground
    | Var of GT.int
  [@@deriving gt ~options:{ show; fmt; gmap }]]
end

[%%distrib
type targ = Typ of targ JType.ground [@@deriving gt ~options:{ show; fmt; gmap }]]

(* type 'a t = Type of 'a [@@deriving gt ~options:{ show; fmt; gmap }]
type ground = ground JType.ground t [@@deriving gt ~options:{ show; fmt; gmap }]

type logic = logic JType.logic t OCanren.logic
[@@deriving gt ~options:{ show; fmt; gmap }]

type injected = injected JType.injected t OCanren.ilogic

let fmapt f__045_ subj__046_ =
  let open OCanren.Env.Monad in
  OCanren.Env.Monad.return (GT.gmap t) <*> f__045_ <*> subj__046_
;;

let (prj_exn : (injected, ground) OCanren.Reifier.t) =
  let open OCanren.Env.Monad in
  OCanren.Reifier.fix (fun self ->
    OCanren.prj_exn <..> chain (fmapt (JType.prj_exn self)))
;;
 *)
