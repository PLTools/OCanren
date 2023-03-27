  $ ../ppx/pp_ocanren_all.exe  test011.ml -pretty #| ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  [@@@ocaml.text
  " This is part of HM inferencer demo from noCanren.\n\
  \  * Look how we are using another extension point which is renaming of 'distrib'\n\
  \  "]

  let () = print_endline "test010"

  module JType = struct
    include struct
      type nonrec ('targ, 'a1, 'a0) t = Array of 'a1 | Var of 'a0
      [@@deriving gt ~options:{show; fmt; gmap}]

      type 'targ ground = ('targ, 'targ ground, GT.int) t [@@deriving gt ~options:{show; fmt; gmap}]

      type 'targ logic = ('targ, 'targ logic, GT.int OCanren.logic) t OCanren.logic
      [@@deriving gt ~options:{show; fmt; gmap}]

      type 'targ injected = ('targ, 'targ injected, GT.int OCanren.ilogic) t OCanren.ilogic

      let fmapt f__006_ f__007_ f__008_ subj__009_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__006_ <*> f__007_ <*> f__008_ <*> subj__009_

      let prj_exn rtarg =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.prj_exn <..> chain (fmapt rtarg self OCanren.prj_exn) )

      let reify rtarg =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt rtarg self OCanren.reify)) ) )

      let array _x__001_ = OCanren.inji (Array _x__001_)

      let var _x__002_ = OCanren.inji (Var _x__002_)
    end
  end
