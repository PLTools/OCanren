  $ ../ppx/pp_ocanren_all.exe  test009.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  [@@@ocaml.text
  " This is part of HM inferencer demo from noCanren.\n\
  \  * Look how we are using another extension point which is renaming of 'distrib'\n\
  \  "]
  
  let () = print_endline "test009"
  
  module _ : sig
    include sig
      type nonrec ('a1, 'a0) t = LInt of 'a1 | LBool of 'a0 [@@deriving gt ~options:{gmap}]
  
      type nonrec ground = (GT.int, GT.bool) t [@@deriving gt ~options:{gmap}]
  
      type nonrec logic = (GT.int OCanren.logic, GT.bool OCanren.logic) t OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
      type nonrec injected = (GT.int OCanren.ilogic, GT.bool OCanren.ilogic) t OCanren.ilogic
  
      val prj_exn : (injected, ground) OCanren.Reifier.t
  
      val reify : (injected, logic) OCanren.Reifier.t
    end
  end = struct
    include struct
      type nonrec ('a1, 'a0) t = LInt of 'a1 | LBool of 'a0 [@@deriving gt ~options:{gmap}]
  
      type nonrec ground = (GT.int, GT.bool) t [@@deriving gt ~options:{gmap}]
  
      type nonrec logic = (GT.int OCanren.logic, GT.bool OCanren.logic) t OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
      type nonrec injected = (GT.int OCanren.ilogic, GT.bool OCanren.ilogic) t OCanren.ilogic
  
      let fmapt f__005_ f__006_ subj__007_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__005_ <*> f__006_ <*> subj__007_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn OCanren.prj_exn) )
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt OCanren.reify OCanren.reify)) ) )
  
      let lInt _x__001_ = OCanren.inj (LInt _x__001_)
  
      let lBool _x__002_ = OCanren.inj (LBool _x__002_)
    end
  end
  
  let () =
    let open OCanren in
    let open Tester in
    run_r OCanren.reify
      (GT.show OCanren.logic (GT.show GT.int))
      1 q Tester.qh
      ("<string repr of goal>", fun q -> q === inj 1) ;
    [%tester run_r OCanren.reify (GT.show OCanren.logic (GT.show GT.int)) 1 (fun q -> q === inj 1)]

  $ ./test009.exe
  test009
  <string repr of goal>, 1 answer {
  q=1;
  }
  fun q -> q === (inj 1), 1 answer {
  q=1;
  }
