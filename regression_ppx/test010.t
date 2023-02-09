  $ ../ppx/pp_ocanren_all.exe  test010.ml -new-typenames -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  [@@@ocaml.text
  " This is part of HM inferencer demo from noCanren.\n\
  \  * Look how we are using another extension point which is renaming of 'distrib'\n\
  \  "]
  
  let () = print_endline "test010"
  
  include struct
    type nonrec state_fuly = S [@@deriving gt ~options:{gmap}]
  
    type nonrec state = state_fuly [@@deriving gt ~options:{gmap}]
  
    type nonrec state_logic = state_fuly OCanren.logic [@@deriving gt ~options:{gmap}]
  
    type nonrec state_injected = state_fuly OCanren.ilogic
  
    let fmapt subj__001_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap state_fuly) <*> subj__001_
  
    let (state_prj_exn : (_, state) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain fmapt)
  
    let (state_reify : (_, state_logic) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ ->
          OCanren.reify <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:fmapt)) )
  end
  
  module _ = struct
    include struct
      type nonrec 'a0 u_fuly = U of 'a0 [@@deriving gt ~options:{gmap}]
  
      type nonrec u = state u_fuly [@@deriving gt ~options:{gmap}]
  
      type nonrec u_logic = state_logic u_fuly OCanren.logic [@@deriving gt ~options:{gmap}]
  
      type nonrec u_injected = state_injected u_fuly OCanren.ilogic
  
      let fmapt f__003_ subj__004_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap u_fuly) <*> f__003_ <*> subj__004_
  
      let (u_prj_exn : (_, u) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain (fmapt state_prj_exn))
  
      let (u_reify : (_, u_logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt state_reify))) )
    end
  end

  $ ./test010.exe
  test010
