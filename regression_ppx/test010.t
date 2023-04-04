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
  
    let (state_prj_exn : (state_injected, state) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain fmapt)
  
    let (state_reify : (state_injected, state_logic) OCanren.Reifier.t) =
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
  
      let (u_prj_exn : (u_injected, u) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain (fmapt state_prj_exn))
  
      let (u_reify : (u_injected, u_logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt state_reify))) )
    end
  end
  
  module Move = struct
    include struct
      type nonrec 'a move_fuly = Forward of 'a | Backward of 'a [@@deriving gt ~options:{gmap}]
  
      type nonrec 'a move = 'a move_fuly [@@deriving gt ~options:{gmap}]
  
      type nonrec 'a move_logic = 'a move_fuly OCanren.logic [@@deriving gt ~options:{gmap}]
  
      type nonrec 'a move_injected = 'a move_fuly OCanren.ilogic
  
      let fmapt f__006_ subj__007_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap move_fuly) <*> f__006_ <*> subj__007_
  
      let move_prj_exn ra =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain (fmapt ra))
  
      let move_reify ra =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra))) )
    end
  end
  
  include struct
    type hum_moves = GT.int Move.move [@@deriving gt ~options:{gmap}]
  
    type hum_moves_logic = GT.int OCanren.logic Move.move_logic [@@deriving gt ~options:{gmap}]
  
    let (reify_hum_moves : (_, GT.int OCanren.logic Move.move_logic) OCanren.Reifier.t) =
      Move.move_reify OCanren.reify
  
    let (prj_exn_hum_moves : (_, GT.int Move.move) OCanren.Reifier.t) =
      Move.move_prj_exn OCanren.prj_exn
  end
  
  include struct
    type asdf = GT.int OCanren.Std.List.list [@@deriving gt ~options:{gmap}]
  
    type asdf_logic = GT.int OCanren.logic OCanren.Std.List.list_logic [@@deriving gt ~options:{gmap}]
  
    let (reify_asdf : (_, GT.int OCanren.logic OCanren.Std.List.list_logic) OCanren.Reifier.t) =
      OCanren.Std.List.list_reify OCanren.reify
  
    let (prj_exn_asdf : (_, GT.int OCanren.Std.List.list) OCanren.Reifier.t) =
      OCanren.Std.List.list_prj_exn OCanren.prj_exn
  end

  $ ./test010.exe
  test010
