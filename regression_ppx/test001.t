  $ pp_distrib test001.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=janestreet -
  open OCanren
  open Tester
  
  let rec zed f x = f (zed f) x
  
  module _ = struct
    include struct
      type nonrec 'a t =
        | Z
        | S of 'a
      [@@deriving gt ~options:{ gmap; show }]
  
      type ground = ground t [@@deriving gt ~options:{ gmap; show }]
      type logic = logic t OCanren.logic [@@deriving gt ~options:{ gmap; show }]
      type injected = injected t OCanren.ilogic
  
      let fmapt a__002_ subj__003_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> a__002_ <*> subj__003_
      ;;
  
      let (prj_exn : (_, ground t) Reifier.t) =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt self))
      ;;
  
      let (reify : (_, logic t OCanren.logic) Reifier.t) =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self ->
            OCanren.reify <..> chain (Reifier.zed (Reifier.rework ~fv:(fmapt self))))
      ;;
  
      let z () = OCanren.inji Z
      let s _x__001_ = OCanren.inji (S _x__001_)
    end
  
    let run_peano_exn n = run_r prj_exn (GT.show ground) n
    let run_peano n = run_r reify (GT.show logic) n
  
    let () =
      run_peano 1 q qh (REPR (fun q -> q === z ()));
      run_peano 1 q qh (REPR (fun q -> q === s (z ())))
    ;;
  end
  
  module _ = struct
    include struct
      type nonrec 'a t =
        | None
        | Some of 'a
      [@@deriving gt ~options:{ gmap; show }]
  
      type nonrec 'a ground = 'a t [@@deriving gt ~options:{ gmap; show }]
      type nonrec 'a logic = 'a t OCanren.logic [@@deriving gt ~options:{ gmap; show }]
      type nonrec 'a injected = 'a t OCanren.ilogic
  
      let fmapt a__005_ subj__006_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> a__005_ <*> subj__006_
      ;;
  
      let prj_exn ra =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt ra))
      ;;
  
      let reify ra =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self ->
            OCanren.reify <..> chain (Reifier.zed (Reifier.rework ~fv:(fmapt ra))))
      ;;
  
      let none () = OCanren.inji None
      let some _x__004_ = OCanren.inji (Some _x__004_)
    end
  
    let run_option n =
      run_r
        [%reify: GT.int ground]
        (GT.show logic (GT.show OCanren.logic (GT.show GT.int)))
        n
    ;;
  
    let () =
      run_option 1 q qh (REPR (fun q -> q === none ()));
      run_option 1 q qh (REPR (fun q -> fresh x (q === some x)));
      run_option 1 q qh (REPR (fun q -> fresh x (q === some !!42)))
    ;;
  end
  
  module _ = struct
    include struct
      type nonrec ('a, 'b) t =
        | [] [@name "nil"]
        | ( :: ) of 'a * 'b [@name "cons"]
      [@@deriving gt ~options:{ gmap; show }]
  
      type 'a ground = ('a, 'a ground) t [@@deriving gt ~options:{ gmap; show }]
      type 'a logic = ('a, 'a logic) t OCanren.logic [@@deriving gt ~options:{ gmap; show }]
      type 'a injected = ('a, 'a injected) t OCanren.ilogic
  
      let fmapt a__009_ b__010_ subj__011_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> a__009_ <*> b__010_ <*> subj__011_
      ;;
  
      let prj_exn ra =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt ra self))
      ;;
  
      let reify ra =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self ->
            OCanren.reify <..> chain (Reifier.zed (Reifier.rework ~fv:(fmapt ra self))))
      ;;
  
      let nil () = OCanren.inji []
      let cons _x__007_ _x__008_ = OCanren.inji (_x__007_ :: _x__008_)
    end
  
    let __ : ('a -> string) -> ('a logic as 'b) -> string = GT.show logic
  
    let __ : int OCanren.logic logic -> string =
      GT.show logic (GT.show OCanren.logic (GT.show GT.int))
    ;;
  
    let __ : ((('b ilogic, 'a) t ilogic as 'a), (('b, 'c) t as 'c)) OCanren__Logic.Reifier.t
      =
      prj_exn OCanren.prj_exn
    ;;
  
    let __ = reify OCanren.reify
  
    let run_list n =
      run_r [%reify: GT.int t] (GT.show logic (GT.show OCanren.logic (GT.show GT.int))) n
    ;;
  
    let () =
      run_list 1 q qh (REPR (fun q -> q === nil ()));
      run_list 1 q qh (REPR (fun q -> fresh x (q === cons x (nil ()))))
    ;;
  end
  
  module Moves = struct
    include struct
      type nonrec 'nat t =
        | Forward of 'nat
        | Backward of 'nat
        | Unload of 'nat
        | Fill of 'nat
      [@@deriving gt ~options:{ gmap; show }]
  
      type nonrec ground = GT.int t [@@deriving gt ~options:{ gmap; show }]
  
      type nonrec logic = GT.int OCanren.logic t OCanren.logic
      [@@deriving gt ~options:{ gmap; show }]
  
      type nonrec injected = GT.int OCanren.ilogic t OCanren.ilogic
  
      let fmapt nat__016_ subj__017_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> nat__016_ <*> subj__017_
      ;;
  
      let (prj_exn : (_, GT.int t) Reifier.t) =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn))
      ;;
  
      let (reify : (_, GT.int OCanren.logic t OCanren.logic) Reifier.t) =
        let open Env.Monad in
        let open Env.Monad.Syntax in
        Reifier.fix (fun self ->
            OCanren.reify
            <..> chain (Reifier.zed (Reifier.rework ~fv:(fmapt OCanren.reify))))
      ;;
  
      let forward _x__012_ = OCanren.inji (Forward _x__012_)
      let backward _x__013_ = OCanren.inji (Backward _x__013_)
      let unload _x__014_ = OCanren.inji (Unload _x__014_)
      let fill _x__015_ = OCanren.inji (Fill _x__015_)
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
  fun q -> fresh x (q === (some (!! 42))), 1 answer {
  q=Some (42);
  }
  fun q -> q === (nil ()), 1 answer {
  q=[];
  }
  fun q -> fresh x (q === (cons x (nil ()))), 1 answer {
  q=:: (_.11, []);
  }
