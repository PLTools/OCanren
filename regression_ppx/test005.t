  $ ../ppx/pp_deriving_reify.exe -pp ../ppx/pp_distrib.exe test005.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat -
  open OCanren
  
  module _ = struct
    type state = (int * int) * (int * int) GT.list [@@deriving reify]
  
    include struct
      let (reify :
            ( _
            , ( (int OCanren.logic * int OCanren.logic) OCanren.logic
              * (int OCanren.logic * int OCanren.logic) OCanren.logic
                OCanren.Std.List.logic )
              OCanren.logic )
            OCanren.Reifier.t ) =
        OCanren.Std.Pair.reify
          (OCanren.Std.Pair.reify OCanren.reify OCanren.reify)
          (OCanren.Std.List.reify
             (OCanren.Std.Pair.reify OCanren.reify OCanren.reify) )
  
      let (prj_exn :
            ( _
            , (int * int) * (int * int) OCanren.Std.List.ground )
            OCanren.Reifier.t ) =
        OCanren.Std.Pair.prj_exn
          (OCanren.Std.Pair.prj_exn OCanren.prj_exn OCanren.prj_exn)
          (OCanren.Std.List.prj_exn
             (OCanren.Std.Pair.prj_exn OCanren.prj_exn OCanren.prj_exn) )
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    let () =
      (let open OCanren in
       run q )
        (fun q -> q === Std.pair (Std.pair !!1 !!2) (Std.nil ()))
        (fun rr -> rr#reify prj_exn)
      |> OCanren.Stream.hd
      |> function (1, 2), [] -> () | _ -> assert false
  end
  
  module _ = struct
    include struct
      type state = (int * int * int) GT.list
  
      type logic =
        (int OCanren.logic * int OCanren.logic * int OCanren.logic) OCanren.logic
        OCanren.Std.List.logic
  
      type injected =
        (int OCanren.ilogic * int OCanren.ilogic * int OCanren.ilogic)
        OCanren.ilogic
        OCanren.Std.List.injected
  
      let (reify :
            ( _
            , (int OCanren.logic * int OCanren.logic * int OCanren.logic)
              OCanren.logic
              OCanren.Std.List.logic )
            OCanren.Reifier.t ) =
        OCanren.Std.List.reify
          ((fun r__008_ r__009_ r__010_ ->
             let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
             let fmapt f__011_ f__012_ f__013_ subj__014_ =
               let open OCanren.Env.Monad in
               OCanren.Env.Monad.return gmap_tuple
               <*> f__011_ <*> f__012_ <*> f__013_ <*> subj__014_
             in
             OCanren.Reifier.fix (fun _ ->
                 let open OCanren.Env.Monad in
                 OCanren.reify
                 <..> chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework
                              ~fv:(fmapt r__008_ r__009_ r__010_) ) ) ) )
             OCanren.reify OCanren.reify OCanren.reify )
  
      let (prj_exn :
            (_, (int * int * int) OCanren.Std.List.ground) OCanren.Reifier.t ) =
        OCanren.Std.List.prj_exn
          ((fun r__001_ r__002_ r__003_ ->
             let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
             let fmapt f__004_ f__005_ f__006_ subj__007_ =
               let open OCanren.Env.Monad in
               OCanren.Env.Monad.return gmap_tuple
               <*> f__004_ <*> f__005_ <*> f__006_ <*> subj__007_
             in
             OCanren.Reifier.fix (fun _ ->
                 let open OCanren.Env.Monad in
                 OCanren.prj_exn <..> chain (fmapt r__001_ r__002_ r__003_) ) )
             OCanren.prj_exn OCanren.prj_exn OCanren.prj_exn )
    end
  
    let () =
      (let open OCanren in
       run q )
        (fun q -> q === Std.List.cons !!(!!1, !!2, !!3) (Std.nil ()))
        (fun rr -> rr#reify prj_exn)
      |> OCanren.Stream.hd
      |> function (1, 2, 3) :: [] -> () | _ -> assert false
  end
  
  let () = print_endline "test005"
  $ ./test005.exe
  test005
