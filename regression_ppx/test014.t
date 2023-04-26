  $ ../ppx/pp_ocanren_all.exe test014mutual.ml -pretty -new-typenames
  let () = print_endline "test014"
  module _ =
    struct
      include
        struct
          type nonrec 'a0 targ_fuly =
            | Type of 'a0 [@@deriving gt ~options:{ show; gmap }]
          type nonrec 'a0 jtype_fuly =
            | Array of 'a0 [@@deriving gt ~options:{ show; gmap }]
          type targ = jtype targ_fuly
          and jtype = targ jtype_fuly
          type targ_logic = jtype_logic targ_fuly OCanren.logic
          and jtype_logic = targ_logic jtype_fuly OCanren.logic
          type targ_injected = jtype_injected targ_fuly OCanren.ilogic
          and jtype_injected = targ_injected jtype_fuly OCanren.ilogic
          let targ_fmapt f__005_ subj__006_ =
            let open OCanren.Env.Monad in
              ((OCanren.Env.Monad.return (GT.gmap targ_fuly)) <*> f__005_) <*>
                subj__006_
          let jtype_fmapt f__002_ subj__003_ =
            let open OCanren.Env.Monad in
              ((OCanren.Env.Monad.return (GT.gmap jtype_fuly)) <*> f__002_) <*>
                subj__003_
          include
            struct
              let rec __jtype fa0 =
                let open OCanren.Env.Monad in
                  OCanren.Reifier.fix
                    (fun _ -> OCanren.prj_exn <..> (chain (jtype_fmapt fa0)))
              and __targ fa0 =
                let open OCanren.Env.Monad in
                  OCanren.Reifier.fix
                    (fun _ -> OCanren.prj_exn <..> (chain (targ_fmapt fa0)))
              let fix =
                let rec jtype_prj_exn eta = (__jtype targ_prj_exn) eta
                and targ_prj_exn eta = (__targ jtype_prj_exn) eta in
                (jtype_prj_exn, targ_prj_exn)
              let jtype_prj_exn eta = let (f, _) = fix in f eta
              let targ_prj_exn eta = let (_, f) = fix in f eta
            end
          include
            struct
              let rec __jtype fa0 =
                let open OCanren.Env.Monad in
                  OCanren.Reifier.fix
                    (fun _ ->
                       OCanren.reify <..>
                         (chain
                            (OCanren.Reifier.zed
                               (OCanren.Reifier.rework ~fv:(jtype_fmapt fa0)))))
              and __targ fa0 =
                let open OCanren.Env.Monad in
                  OCanren.Reifier.fix
                    (fun _ ->
                       OCanren.reify <..>
                         (chain
                            (OCanren.Reifier.zed
                               (OCanren.Reifier.rework ~fv:(targ_fmapt fa0)))))
              let fix =
                let rec jtype_reify eta = (__jtype targ_reify) eta
                and targ_reify eta = (__targ jtype_reify) eta in
                (jtype_reify, targ_reify)
              let jtype_reify eta = let (f, _) = fix in f eta
              let targ_reify eta = let (_, f) = fix in f eta
            end
        end
    end
  $ ./test014mutual.exe
  test014
