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
          type u_injected = state_injected u_fuly OCanren.ilogic
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
  module Move =
    struct
      include
        struct
          type nonrec 'a move_fuly =
            | Forward of 'a 
            | Backward of 'a [@@deriving gt ~options:{ gmap; show }]
          type 'a move = 'a move_fuly[@@deriving gt ~options:{ gmap; show }]
          type 'a move_logic = 'a move_fuly OCanren.logic[@@deriving
                                                           gt
                                                             ~options:{
                                                                      gmap;
                                                                      show
                                                                      }]
          type 'a move_injected = 'a move_fuly OCanren.ilogic
          let fmapt f__006_ subj__007_ =
            let open OCanren.Env.Monad in
              ((OCanren.Env.Monad.return (GT.gmap move_fuly)) <*> f__006_) <*>
                subj__007_
          let move_prj_exn ra =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun self -> OCanren.prj_exn <..> (chain (fmapt ra)))
          let move_reify ra =
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
      type hum_moves = GT.int Move.move[@@deriving gt ~options:{ gmap; show }]
      let (reify_hum_moves :
        (_, GT.int OCanren.logic Move.move_logic) OCanren.Reifier.t) =
        Move.move_reify OCanren.reify
      let (prj_exn_hum_moves : (_, GT.int Move.move) OCanren.Reifier.t) =
        Move.move_prj_exn OCanren.prj_exn
    end
