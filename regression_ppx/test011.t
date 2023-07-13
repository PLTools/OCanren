  $ ../ppx/pp_ocanren_all.exe  test011.ml -pretty #| ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  [@@@ocaml.text
    " This test demostrated that we need type annotation near prj_exn/reify\nto avoid compilation error about `_weak` type variables\n  "]
  let () = print_endline "test010"
  module JType =
    struct
      include
        struct
          type nonrec ('targ, 'a1, 'a0) t =
            | Array of 'a1 
            | Var of 'a0 [@@deriving gt ~options:{ show; fmt; gmap }]
          type 'targ ground = ('targ, 'targ ground, GT.int) t[@@deriving
                                                               gt
                                                                 ~options:
                                                                 {
                                                                   show;
                                                                   fmt;
                                                                   gmap
                                                                 }]
          type 'targ logic =
            ('targ, 'targ logic, GT.int OCanren.logic) t OCanren.logic[@@deriving
                                                                      gt
                                                                      ~options:
                                                                      {
                                                                      show;
                                                                      fmt;
                                                                      gmap
                                                                      }]
          type 'targ injected =
            ('targ, 'targ injected, GT.int OCanren.ilogic) t OCanren.ilogic
          let fmapt f__006_ f__007_ f__008_ subj__009_ =
            let open OCanren.Env.Monad in
              ((((OCanren.Env.Monad.return (GT.gmap t)) <*> f__006_) <*>
                  f__007_)
                 <*> f__008_)
                <*> subj__009_
          let (prj_exn :
            ('targ, 'targ_2) OCanren.Reifier.t ->
              ('targ injected, 'targ_2 ground) OCanren.Reifier.t)
            =
            fun rtarg ->
              let open OCanren.Env.Monad in
                OCanren.Reifier.fix
                  (fun self ->
                     OCanren.prj_exn <..>
                       (chain (fmapt rtarg self OCanren.prj_exn)))
          let (reify :
            ('targ, 'targ_2) OCanren.Reifier.t ->
              ('targ injected, 'targ_2 logic) OCanren.Reifier.t)
            =
            fun rtarg ->
              let open OCanren.Env.Monad in
                OCanren.Reifier.fix
                  (fun self ->
                     OCanren.reify <..>
                       (chain
                          (OCanren.Reifier.zed
                             (OCanren.Reifier.rework
                                ~fv:(fmapt rtarg self OCanren.reify)))))
          let array _x__001_ = OCanren.inji (Array _x__001_)
          let var _x__002_ = OCanren.inji (Var _x__002_)
        end
    end
  include
    struct
      type nonrec 'a0 t =
        | Typ of 'a0 [@@deriving gt ~options:{ show; fmt; gmap }]
      type ground = ground JType.ground t[@@deriving
                                           gt ~options:{ show; fmt; gmap }]
      type logic = logic JType.logic t OCanren.logic[@@deriving
                                                      gt
                                                        ~options:{
                                                                   show;
                                                                   fmt;
                                                                   gmap
                                                                 }]
      type injected = injected JType.injected t OCanren.ilogic
      let fmapt f__012_ subj__013_ =
        let open OCanren.Env.Monad in
          ((OCanren.Env.Monad.return (GT.gmap t)) <*> f__012_) <*> subj__013_
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun self ->
               OCanren.prj_exn <..> (chain (fmapt (JType.prj_exn self))))
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun self ->
               OCanren.reify <..>
                 (chain
                    (OCanren.Reifier.zed
                       (OCanren.Reifier.rework ~fv:(fmapt (JType.reify self))))))
      let typ _x__010_ = OCanren.inji (Typ _x__010_)
    end
