  $ ../ppx/pp_distrib.exe test001.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat -
  open OCanren
  open Tester
  
  module _ = struct
    include struct
      type nonrec 'a t = Z | S of 'a [@@deriving gt ~options:{gmap; show}]
  
      type ground = ground t [@@deriving gt ~options:{gmap; show}]
  
      type logic = logic t OCanren.logic [@@deriving gt ~options:{gmap; show}]
  
      type injected = injected t OCanren.ilogic
  
      let fmapt f__003_ subj__004_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__003_ <*> subj__004_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt self))
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt self)) ) )
  
      let z () = OCanren.inji Z
  
      let s _x__001_ = OCanren.inji (S _x__001_)
    end
  
    let run_peano_exn n = run_r prj_exn (GT.show ground) n
  
    let run_peano n = run_r reify (GT.show logic) n
  
    let () =
      run_peano 1 q qh (REPR (fun q -> q === z ())) ;
      run_peano 1 q qh (REPR (fun q -> q === s (z ())))
  end
  
  module _ = struct
    include struct
      type nonrec 'a t = None | Some of 'a [@@deriving gt ~options:{gmap; show}]
  
      type nonrec 'a ground = 'a t [@@deriving gt ~options:{gmap; show}]
  
      type nonrec 'a logic = 'a t OCanren.logic
      [@@deriving gt ~options:{gmap; show}]
  
      type nonrec 'a injected = 'a t OCanren.ilogic
  
      let fmapt f__007_ subj__008_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__007_ <*> subj__008_
  
      let (prj_exn :
               ('a, 'a_2) OCanren.Reifier.t
            -> ('a injected, 'a_2 ground) OCanren.Reifier.t ) =
       fun ra ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain (fmapt ra))
  
      let (reify :
               ('a, 'a_2) OCanren.Reifier.t
            -> ('a injected, 'a_2 logic) OCanren.Reifier.t ) =
       fun ra ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra))) )
  
      let none () = OCanren.inji None
  
      let some _x__005_ = OCanren.inji (Some _x__005_)
    end
  
    let run_option n =
      run_r [%reify: GT.int ground]
        (GT.show logic (GT.show OCanren.logic (GT.show GT.int)))
        n
  
    let () =
      run_option 1 q qh (REPR (fun q -> q === none ())) ;
      run_option 1 q qh (REPR (fun q -> fresh x (q === some x))) ;
      run_option 1 q qh (REPR (fun q -> fresh () (q === some !!42)))
  end
  
  module _ = struct
    include struct
      type nonrec ('a, 'b) t =
        | [] [@name "nil"]
        | ( :: ) of 'a * 'b [@name "cons"]
      [@@deriving gt ~options:{gmap; show}]
  
      type 'a ground = ('a, 'a ground) t [@@deriving gt ~options:{gmap; show}]
  
      type 'a logic = ('a, 'a logic) t OCanren.logic
      [@@deriving gt ~options:{gmap; show}]
  
      type 'a injected = ('a, 'a injected) t OCanren.ilogic
  
      let fmapt f__013_ f__014_ subj__015_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t)
        <*> f__013_ <*> f__014_ <*> subj__015_
  
      let (prj_exn :
               ('a, 'a_2) OCanren.Reifier.t
            -> ('a injected, 'a_2 ground) OCanren.Reifier.t ) =
       fun ra ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.prj_exn <..> chain (fmapt ra self) )
  
      let (reify :
               ('a, 'a_2) OCanren.Reifier.t
            -> ('a injected, 'a_2 logic) OCanren.Reifier.t ) =
       fun ra ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt ra self)) ) )
  
      let nil () = OCanren.inji []
  
      let cons _x__009_ _x__010_ = OCanren.inji (_x__009_ :: _x__010_)
    end
  
    let __ : ('a -> string) -> ('a logic as 'b) -> string = GT.show logic
  
    let __ : int OCanren.logic logic -> string =
      GT.show logic (GT.show OCanren.logic (GT.show GT.int))
  
    let __ :
        ( (('b ilogic, 'a) t ilogic as 'a)
        , (('b, 'c) t as 'c) )
        OCanren__Logic.Reifier.t =
      prj_exn OCanren.prj_exn
  
    let __ = reify OCanren.reify
  
    let run_list n =
      run_r [%reify: GT.int t]
        (GT.show logic (GT.show OCanren.logic (GT.show GT.int)))
        n
  
    let () =
      run_list 1 q qh (REPR (fun q -> q === nil ())) ;
      run_list 1 q qh (REPR (fun q -> fresh x (q === cons x (nil ()))))
  end
  
  module Moves = struct
    include struct
      type nonrec 'nat t =
        | Forward of 'nat
        | Backward of 'nat
        | Unload of 'nat
        | Fill of 'nat
      [@@deriving gt ~options:{gmap; show}]
  
      type nonrec ground = GT.int t [@@deriving gt ~options:{gmap; show}]
  
      type nonrec logic = GT.int OCanren.logic t OCanren.logic
      [@@deriving gt ~options:{gmap; show}]
  
      type nonrec injected = GT.int OCanren.ilogic t OCanren.ilogic
  
      let fmapt f__021_ subj__022_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__021_ <*> subj__022_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn) )
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt OCanren.reify)) ) )
  
      let forward _x__016_ = OCanren.inji (Forward _x__016_)
  
      let backward _x__017_ = OCanren.inji (Backward _x__017_)
  
      let unload _x__018_ = OCanren.inji (Unload _x__018_)
  
      let fill _x__019_ = OCanren.inji (Fill _x__019_)
    end
  end
  
  module _ = struct
    include struct
      type nonrec ('a, 'b, 'c) t = 'a * 'b * 'c
      [@@deriving gt ~options:{gmap; show}]
  
      type nonrec ('a, 'b, 'c) ground = ('a, 'b, 'c) t
      [@@deriving gt ~options:{gmap; show}]
  
      type nonrec ('a, 'b, 'c) logic = ('a, 'b, 'c) t OCanren.logic
      [@@deriving gt ~options:{gmap; show}]
  
      type nonrec ('a, 'b, 'c) injected = ('a, 'b, 'c) t OCanren.ilogic
  
      let fmapt f__026_ f__027_ f__028_ subj__029_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t)
        <*> f__026_ <*> f__027_ <*> f__028_ <*> subj__029_
  
      let (prj_exn :
               ('a, 'a_2) OCanren.Reifier.t
            -> ('b, 'b_2) OCanren.Reifier.t
            -> ('c, 'c_2) OCanren.Reifier.t
            -> ( ('a, 'b, 'c) injected
               , ('a_2, 'b_2, 'c_2) ground )
               OCanren.Reifier.t ) =
       fun ra rb rc ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain (fmapt ra rb rc))
  
      let (reify :
               ('a, 'a_2) OCanren.Reifier.t
            -> ('b, 'b_2) OCanren.Reifier.t
            -> ('c, 'c_2) OCanren.Reifier.t
            -> (('a, 'b, 'c) injected, ('a_2, 'b_2, 'c_2) logic) OCanren.Reifier.t
            ) =
       fun ra rb rc ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt ra rb rc)) ) )
    end
  end
  $ ./test001.exe
  fun q -> q === (z ()), 1 answer {
  q=Z;
  }
  fun q -> q === (s (z ())), 1 answer {
  q=S (Z);
  }
  fun q -> q === (none ()), 1 answer {
  q=None;
  }
  fun q -> fresh x (q === (some x)), 1 answer {
  q=Some (_.11);
  }
  fun q -> fresh () (q === (some (!! 42))), 1 answer {
  q=Some (42);
  }
  fun q -> q === (nil ()), 1 answer {
  q=[];
  }
  fun q -> fresh x (q === (cons x (nil ()))), 1 answer {
  q=:: (_.11, []);
  }
