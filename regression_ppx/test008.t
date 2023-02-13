  $ ../ppx/pp_ocanren_all.exe  test008.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  let () = print_endline "test008"
  
  module _ = struct
    include struct
      type nonrec ('a, 'a0) t = Cons of 'a * 'a0 [@@deriving gt ~options:{show; gmap}]
  
      type 'a ground = ('a, 'a ground) t [@@deriving gt ~options:{show; gmap}]
  
      type 'a logic = ('a, 'a logic) t OCanren.logic [@@deriving gt ~options:{show; gmap}]
  
      type 'a injected = ('a, 'a injected) t OCanren.ilogic
  
      let fmapt f__005_ f__006_ subj__007_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__005_ <*> f__006_ <*> subj__007_
  
      let prj_exn ra =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt ra self))
  
      let reify ra =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.reify
            <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra self))) )
  
      let cons _x__001_ _x__002_ = OCanren.inji (Cons (_x__001_, _x__002_))
    end
  end
  
  module _ = struct
    include struct
      type nonrec ('a, 'a0) t = {name: 'a0; v: 'a} [@@deriving gt ~options:{show; gmap}]
  
      type nonrec 'a ground = ('a, GT.int) t [@@deriving gt ~options:{show; gmap}]
  
      type nonrec 'a logic = ('a, GT.int OCanren.logic) t OCanren.logic
      [@@deriving gt ~options:{show; gmap}]
  
      type nonrec 'a injected = ('a, GT.int OCanren.ilogic) t OCanren.ilogic
  
      let fmapt f__010_ f__011_ subj__012_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__010_ <*> f__011_ <*> subj__012_
  
      let prj_exn ra =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain (fmapt ra OCanren.prj_exn))
  
      let reify ra =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra OCanren.reify))) )
  
      let make_t name v = OCanren.inji {name; v}
    end
  end
  $ ./test008.exe
  test008
