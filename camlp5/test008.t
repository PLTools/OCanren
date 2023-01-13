  $ ../ppx/pp_distrib.exe -pp ./pp5+ocanren+o.exe -pretty -new-typenames test008.ml # | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  include
    struct
      type nonrec state_fuly =
        | S [@@deriving gt ~options:{ gmap; show }]
      type state = state_fuly[@@deriving gt ~options:{ gmap; show }]
      type state_logic = state_fuly OCanren.logic[@@deriving
                                                   gt ~options:{ gmap; show }]
      type state_injected = state
      let fmapt subj__001_ =
        let open OCanren.Env.Monad in
          (OCanren.Env.Monad.return (GT.gmap state_fuly)) <*> subj__001_
      let (state_prj_exn : (_, state) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> (chain fmapt))
      let (state_reify : (_, state_logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun self ->
               OCanren.reify <..>
                 (chain
                    (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:fmapt))))
    end
  module _ =
    struct
      include
        struct
          type nonrec 'a0 u_fuly =
            | U of 'a0 [@@deriving gt ~options:{ gmap; show }]
          type u = state u_fuly[@@deriving gt ~options:{ gmap; show }]
          type u_logic = state_logic u_fuly OCanren.logic[@@deriving
                                                           gt
                                                             ~options:{
                                                                      gmap;
                                                                      show
                                                                      }]
          type u_injected = u
          let fmapt f__003_ subj__004_ =
            let open OCanren.Env.Monad in
              ((OCanren.Env.Monad.return (GT.gmap u_fuly)) <*> f__003_) <*>
                subj__004_
          let (u_prj_exn : (_, u) OCanren.Reifier.t) =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self -> OCanren.prj_exn <..> (chain (fmapt state_prj_exn)))
          let (u_reify : (_, u_logic) OCanren.Reifier.t) =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self ->
                   OCanren.reify <..>
                     (chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(fmapt state_reify)))))
        end
    end
