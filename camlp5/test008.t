  $ ../ppx/pp_distrib.exe -pp ./pp5+ocanren+o.exe -pretty -new-typenames test008.ml # | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  include
    struct
      type nonrec state_fuly =
        | S [@@deriving gt ~options:{ gmap; show }]
      type state = state_fuly[@@deriving gt ~options:{ gmap; show }]
      type state_logic = state_fuly OCanren.logic[@@deriving
                                                   gt ~options:{ gmap; show }]
      type state_injected = state_fuly OCanren.ilogic
      let fmapt subj__001_ =
        let open OCanren.Env.Monad in
          (OCanren.Env.Monad.return (GT.gmap state_fuly)) <*> subj__001_
      let (state_prj_exn : (state_injected, state) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> (chain fmapt))
      let (state_reify : (state_injected, state_logic) OCanren.Reifier.t) =
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
          type u_injected = state_injected u_fuly OCanren.ilogic
          let fmapt f__003_ subj__004_ =
            let open OCanren.Env.Monad in
              ((OCanren.Env.Monad.return (GT.gmap u_fuly)) <*> f__003_) <*>
                subj__004_
          let (u_prj_exn : (u_injected, u) OCanren.Reifier.t) =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self -> OCanren.prj_exn <..> (chain (fmapt state_prj_exn)))
          let (u_reify : (u_injected, u_logic) OCanren.Reifier.t) =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self ->
                   OCanren.reify <..>
                     (chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(fmapt state_reify)))))
        end
    end
  module Move =
    struct
      include
        struct
          type nonrec 'a ground_fuly =
            | Forward of 'a 
            | Backward of 'a [@@deriving gt ~options:{ gmap; show }]
          type 'a ground = 'a ground_fuly[@@deriving
                                           gt ~options:{ gmap; show }]
          type 'a ground_logic = 'a ground_fuly OCanren.logic[@@deriving
                                                               gt
                                                                 ~options:
                                                                 { gmap; show
                                                                 }]
          type 'a ground_injected = 'a ground_fuly OCanren.ilogic
          let fmapt f__006_ subj__007_ =
            let open OCanren.Env.Monad in
              ((OCanren.Env.Monad.return (GT.gmap ground_fuly)) <*> f__006_)
                <*> subj__007_
          let ground_prj_exn ra =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self -> OCanren.prj_exn <..> (chain (fmapt ra)))
          let ground_reify ra =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self ->
                   OCanren.reify <..>
                     (chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(fmapt ra)))))
        end
    end
  include
    struct
      type hum_moves = GT.int Move.ground[@@deriving
                                           gt ~options:{ gmap; show }]
      type hum_moves_logic = GT.int OCanren.logic Move.ground_logic[@@deriving
                                                                     gt
                                                                      ~options:
                                                                      {
                                                                      gmap;
                                                                      show
                                                                      }]
      let (reify_hum_moves :
        (_, GT.int OCanren.logic Move.ground_logic) OCanren.Reifier.t) =
        Move.ground_reify OCanren.reify
      let (prj_exn_hum_moves : (_, GT.int Move.ground) OCanren.Reifier.t) =
        Move.ground_prj_exn OCanren.prj_exn
    end
