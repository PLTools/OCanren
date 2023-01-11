  $ pp_deriving_reify test005.ml | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat -
  open OCanren
  
  module _ = struct
    type state = (int * int) * (int * int) GT.list [@@deriving reify]
  
    include struct
      let _ = fun (_ : state) -> ()
  
      let (reify_state :
            ( _
            , ( (int OCanren.logic, int OCanren.logic) OCanren.Std.Pair.logic
              , (int OCanren.logic, int OCanren.logic) OCanren.Std.Pair.logic
                OCanren.Std.List.logic )
              OCanren.Std.Pair.logic )
            OCanren.Reifier.t ) =
        OCanren.Std.Pair.reify
          (OCanren.Std.Pair.reify OCanren.reify OCanren.reify)
          (Std.List.reify (OCanren.Std.Pair.reify OCanren.reify OCanren.reify))
  
      let _ = reify_state
  
      let (prj_exn_state :
            ( _
            , ( (int, int) OCanren.Std.Pair.ground
              , (int, int) OCanren.Std.Pair.ground OCanren.Std.List.ground )
              OCanren.Std.Pair.ground )
            OCanren.Reifier.t ) =
        OCanren.Std.Pair.prj_exn
          (OCanren.Std.Pair.prj_exn OCanren.prj_exn OCanren.prj_exn)
          (Std.List.prj_exn
             (OCanren.Std.Pair.prj_exn OCanren.prj_exn OCanren.prj_exn) )
  
      let _ = prj_exn_state
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  
  let () = print_endline "test005"
  $ ./test005.exe
  test005
