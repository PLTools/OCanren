  $ ../ppx/pp_gt.exe -pp pp_ocanren_all test007.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  open OCanren
  
  let () = print_endline "test007"
  
  module Scheme = struct
    include struct
      type nonrec ('a1, 'a0) t = Symb of 'a1 | Seq of 'a0 [@@deriving gt ~options:{show; gmap}]
  
      include struct
        class virtual ['ia1, 'a1, 'sa1, 'ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] t_t =
          object
            method virtual c_Symb : 'inh -> 'extra -> 'a1 -> 'syn
  
            method virtual c_Seq : 'inh -> 'extra -> 'a0 -> 'syn
          end
  
        let gcata_t
            (tr : (_, 'typ0__003_, _, _, 'typ1__004_, _, _, ('typ0__003_, 'typ1__004_) t, _) #t_t) inh
            subj =
          match subj with
          | Symb _x__001_ ->
              tr#c_Symb inh subj _x__001_
          | Seq _x__002_ ->
              tr#c_Seq inh subj _x__002_
  
        class ['a1, 'a1_2, 'a0, 'a0_2, 'extra_t, 'syn_t] gmap_t_t fa1 fa0 =
          let _ = fa1 in
          fun _fself_t ->
            object
              inherit [unit, 'a1, 'a1_2, unit, 'a0, 'a0_2, unit, 'extra_t, ('a1_2, 'a0_2) t] t_t
  
              constraint 'extra_t = ('a1, 'a0) t
  
              constraint 'syn_t = ('a1_2, 'a0_2) t
  
              method c_Symb () _ _x__005_ = Symb (fa1 () _x__005_)
  
              method c_Seq () _ _x__006_ = Seq (fa0 () _x__006_)
            end
  
        let gmap_t fa1 fa0 =
          let _ = fa1 in
          fun inh0 subj -> GT.transform_gc gcata_t (new gmap_t_t fa1 fa0) inh0 subj
  
        class ['a1, 'a0, 'extra_t] show_t_t fa1 fa0 =
          let _ = fa1 in
          fun _fself_t ->
            object
              inherit [unit, 'a1, string, unit, 'a0, string, unit, 'extra_t, string] t_t
  
              constraint 'extra_t = ('a1, 'a0) t
  
              method c_Symb () _ _x__007_ =
                let () = () in
                Printf.sprintf "Symb (%s)" (fa1 () _x__007_)
  
              method c_Seq () _ _x__008_ =
                let () = () in
                Printf.sprintf "Seq (%s)" (fa0 () _x__008_)
            end
  
        let show_t fa1 fa0 =
          let _ = fa1 in
          fun inh0 subj -> GT.transform_gc gcata_t (new show_t_t fa1 fa0) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method gmap fa1 fa0 subj = gmap_t (GT.lift fa1) (GT.lift fa0) () subj
  
                method show fa1 fa0 subj = show_t (GT.lift fa1) (GT.lift fa0) () subj
              end }
  
        let gmap_t fa1 fa0 subj = gmap_t (GT.lift fa1) (GT.lift fa0) () subj
  
        let show_t fa1 fa0 subj = show_t (GT.lift fa1) (GT.lift fa0) () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type ground = (GT.string, ground Std.List.ground) t [@@deriving gt ~options:{show; gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] ground_t =
          object
            inherit
              [ GT.string
              , GT.string
              , GT.string
              , ground Std.List.ground
              , ground Std.List.ground
              , ground Std.List.ground
              , 'inh
              , 'extra
              , 'syn ]
              t_t
          end
  
        let gcata_ground = gcata_t
  
        class ['extra_ground, 'syn_ground] gmap_ground_t _fself_ground =
          object
            inherit [unit, 'extra_ground, ground] ground_t
  
            constraint 'extra_ground = ground
  
            constraint 'syn_ground = ground
  
            inherit
              [ GT.string
              , GT.string
              , ground Std.List.ground
              , ground Std.List.ground
              , 'extra_ground
              , 'syn_ground ]
              gmap_t_t
                (fun () subj -> GT.gmap GT.string subj)
                (fun () subj -> GT.gmap Std.List.ground (_fself_ground ()) subj)
                _fself_ground
          end
  
        let rec gmap_ground () subj =
          GT.gmap t
            ((fun () subj -> GT.gmap GT.string subj) ())
            ((fun () subj -> GT.gmap Std.List.ground (gmap_ground ()) subj) ())
            subj
  
        class ['extra_ground] show_ground_t _fself_ground =
          object
            inherit [unit, 'extra_ground, string] ground_t
  
            constraint 'extra_ground = ground
  
            inherit
              [GT.string, ground Std.List.ground, 'extra_ground] show_t_t
                (fun () subj -> GT.show GT.string subj)
                (fun () subj -> GT.show Std.List.ground (_fself_ground ()) subj)
                _fself_ground
          end
  
        let rec show_ground () subj =
          GT.show t
            ((fun () subj -> GT.show GT.string subj) ())
            ((fun () subj -> GT.show Std.List.ground (show_ground ()) subj) ())
            subj
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_ground () subj
  
                method show subj = show_ground () subj
              end }
  
        let gmap_ground subj = gmap_ground () subj
  
        let show_ground subj = show_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type logic = (GT.string OCanren.logic, logic OCanren.Std.List.logic) t OCanren.logic
      [@@deriving gt ~options:{show; gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit
              [ (GT.string OCanren.logic, logic OCanren.Std.List.logic) t
              , (GT.string OCanren.logic, logic OCanren.Std.List.logic) t
              , (GT.string OCanren.logic, logic OCanren.Std.List.logic) t
              , 'inh
              , 'extra
              , 'syn ]
              OCanren.logic_t
          end
  
        let gcata_logic = OCanren.gcata_logic
  
        class ['extra_logic, 'syn_logic] gmap_logic_t _fself_logic =
          object
            inherit [unit, 'extra_logic, logic] logic_t
  
            constraint 'extra_logic = logic
  
            constraint 'syn_logic = logic
  
            inherit
              [ (GT.string OCanren.logic, logic OCanren.Std.List.logic) t
              , (GT.string OCanren.logic, logic OCanren.Std.List.logic) t
              , 'extra_logic
              , 'syn_logic ]
              OCanren.gmap_logic_t
                (fun () subj ->
                  GT.gmap t
                    ((fun () subj ->
                       GT.gmap OCanren.logic ((fun () subj -> GT.gmap GT.string subj) ()) subj )
                       () )
                    ((fun () subj -> GT.gmap OCanren.Std.List.logic (_fself_logic ()) subj) ())
                    subj )
                _fself_logic
          end
  
        let rec gmap_logic () subj =
          GT.gmap OCanren.logic
            ((fun () subj ->
               GT.gmap t
                 ((fun () subj ->
                    GT.gmap OCanren.logic ((fun () subj -> GT.gmap GT.string subj) ()) subj )
                    () )
                 ((fun () subj -> GT.gmap OCanren.Std.List.logic (gmap_logic ()) subj) ())
                 subj )
               () )
            subj
  
        class ['extra_logic] show_logic_t _fself_logic =
          object
            inherit [unit, 'extra_logic, string] logic_t
  
            constraint 'extra_logic = logic
  
            inherit
              [(GT.string OCanren.logic, logic OCanren.Std.List.logic) t, 'extra_logic] OCanren
                                                                                        .show_logic_t
                (fun () subj ->
                  GT.show t
                    ((fun () subj ->
                       GT.show OCanren.logic ((fun () subj -> GT.show GT.string subj) ()) subj )
                       () )
                    ((fun () subj -> GT.show OCanren.Std.List.logic (_fself_logic ()) subj) ())
                    subj )
                _fself_logic
          end
  
        let rec show_logic () subj =
          GT.show OCanren.logic
            ((fun () subj ->
               GT.show t
                 ((fun () subj ->
                    GT.show OCanren.logic ((fun () subj -> GT.show GT.string subj) ()) subj )
                    () )
                 ((fun () subj -> GT.show OCanren.Std.List.logic (show_logic ()) subj) ())
                 subj )
               () )
            subj
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_logic () subj
  
                method show subj = show_logic () subj
              end }
  
        let gmap_logic subj = gmap_logic () subj
  
        let show_logic subj = show_logic () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type injected = (GT.string OCanren.ilogic, injected OCanren.Std.List.injected) t OCanren.ilogic
  
      let fmapt f__005_ f__006_ subj__007_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__005_ <*> f__006_ <*> subj__007_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn (Std.List.prj_exn self)) )
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt OCanren.reify (Std.List.reify self))) ) )
  
      let symb _x__001_ = OCanren.inji (Symb _x__001_)
  
      let seq _x__002_ = OCanren.inji (Seq _x__002_)
    end
  
    let rec inhabit_list el xs =
      conde
        [ xs === Std.nil ()
        ; Fresh.two (fun h tl -> xs === Std.List.cons h tl &&& el h &&& inhabit_list el tl) ]
  
    let rec inhabit q =
      conde
        [ Fresh.one (fun s -> q === symb s)
        ; Fresh.two (fun _ xs -> q === seq xs &&& inhabit_list inhabit xs) ]
  
    let () =
      print_endline "Inhabitants of scheme terms:" ;
      run q inhabit (fun rr -> rr#reify reify)
      |> Stream.map (GT.show logic)
      |> Stream.take ~n:10
      |> List.iteri (Printf.printf "%2d: %s\n")
  end
  
  module Scheme_rez = struct
    include struct
      type nonrec ('a1, 'a0) t = Val of 'a0 | Closure of 'a1 * 'a0
      [@@deriving gt ~options:{gmap; show}]
  
      include struct
        class virtual ['ia1, 'a1, 'sa1, 'ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] t_t =
          object
            method virtual c_Val : 'inh -> 'extra -> 'a0 -> 'syn
  
            method virtual c_Closure : 'inh -> 'extra -> 'a1 -> 'a0 -> 'syn
          end
  
        let gcata_t
            (tr : (_, 'typ0__012_, _, _, 'typ1__013_, _, _, ('typ0__012_, 'typ1__013_) t, _) #t_t) inh
            subj =
          match subj with
          | Val _x__009_ ->
              tr#c_Val inh subj _x__009_
          | Closure (_x__010_, _x__011_) ->
              tr#c_Closure inh subj _x__010_ _x__011_
  
        class ['a1, 'a0, 'extra_t] show_t_t fa1 fa0 _fself_t =
          object
            inherit [unit, 'a1, string, unit, 'a0, string, unit, 'extra_t, string] t_t
  
            constraint 'extra_t = ('a1, 'a0) t
  
            method c_Val () _ _x__014_ =
              let () = () in
              Printf.sprintf "Val (%s)" (fa0 () _x__014_)
  
            method c_Closure () _ _x__015_ _x__016_ =
              let () = () in
              Printf.sprintf "Closure (%s, %s)" (fa1 () _x__015_) (fa0 () _x__016_)
          end
  
        let show_t fa1 fa0 inh0 subj = GT.transform_gc gcata_t (new show_t_t fa1 fa0) inh0 subj
  
        class ['a1, 'a1_2, 'a0, 'a0_2, 'extra_t, 'syn_t] gmap_t_t fa1 fa0 _fself_t =
          object
            inherit [unit, 'a1, 'a1_2, unit, 'a0, 'a0_2, unit, 'extra_t, ('a1_2, 'a0_2) t] t_t
  
            constraint 'extra_t = ('a1, 'a0) t
  
            constraint 'syn_t = ('a1_2, 'a0_2) t
  
            method c_Val () _ _x__017_ = Val (fa0 () _x__017_)
  
            method c_Closure () _ _x__018_ _x__019_ = Closure (fa1 () _x__018_, fa0 () _x__019_)
          end
  
        let gmap_t fa1 fa0 inh0 subj = GT.transform_gc gcata_t (new gmap_t_t fa1 fa0) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method show fa1 fa0 subj = show_t (GT.lift fa1) (GT.lift fa0) () subj
  
                method gmap fa1 fa0 subj = gmap_t (GT.lift fa1) (GT.lift fa0) () subj
              end }
  
        let show_t fa1 fa0 subj = show_t (GT.lift fa1) (GT.lift fa0) () subj
  
        let gmap_t fa1 fa0 subj = gmap_t (GT.lift fa1) (GT.lift fa0) () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec ground = ((GT.string * Scheme.ground) Std.List.ground, Scheme.ground) t
      [@@deriving gt ~options:{gmap; show}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] ground_t =
          object
            inherit
              [ (GT.string * Scheme.ground) Std.List.ground
              , (GT.string * Scheme.ground) Std.List.ground
              , (GT.string * Scheme.ground) Std.List.ground
              , Scheme.ground
              , Scheme.ground
              , Scheme.ground
              , 'inh
              , 'extra
              , 'syn ]
              t_t
          end
  
        let gcata_ground = gcata_t
  
        class ['extra_ground] show_ground_t _fself_ground =
          object
            inherit [unit, 'extra_ground, string] ground_t
  
            constraint 'extra_ground = ground
  
            inherit
              [(GT.string * Scheme.ground) Std.List.ground, Scheme.ground, 'extra_ground] show_t_t
                (fun () subj ->
                  GT.show Std.List.ground
                    ((fun _x__023_ (_x__024_, _x__025_) ->
                       let () = _x__023_ in
                       Printf.sprintf "(%s, %s)"
                         ((fun () subj -> GT.show GT.string subj) () _x__024_)
                         ((fun () subj -> GT.show Scheme.ground subj) () _x__025_) )
                       () )
                    subj )
                (fun () subj -> GT.show Scheme.ground subj)
                _fself_ground
          end
  
        let show_ground () subj =
          GT.show t
            ((fun () subj ->
               GT.show Std.List.ground
                 ((fun _x__020_ (_x__021_, _x__022_) ->
                    let () = _x__020_ in
                    Printf.sprintf "(%s, %s)"
                      ((fun () subj -> GT.show GT.string subj) () _x__021_)
                      ((fun () subj -> GT.show Scheme.ground subj) () _x__022_) )
                    () )
                 subj )
               () )
            ((fun () subj -> GT.show Scheme.ground subj) ())
            subj
  
        class ['extra_ground, 'syn_ground] gmap_ground_t _fself_ground =
          object
            inherit [unit, 'extra_ground, ground] ground_t
  
            constraint 'extra_ground = ground
  
            constraint 'syn_ground = ground
  
            inherit
              [ (GT.string * Scheme.ground) Std.List.ground
              , (GT.string * Scheme.ground) Std.List.ground
              , Scheme.ground
              , Scheme.ground
              , 'extra_ground
              , 'syn_ground ]
              gmap_t_t
                (fun () subj ->
                  GT.gmap Std.List.ground
                    ((fun _x__029_ (_x__030_, _x__031_) ->
                       ( (fun () subj -> GT.gmap GT.string subj) _x__029_ _x__030_
                       , (fun () subj -> GT.gmap Scheme.ground subj) _x__029_ _x__031_ ) )
                       () )
                    subj )
                (fun () subj -> GT.gmap Scheme.ground subj)
                _fself_ground
          end
  
        let gmap_ground () subj =
          GT.gmap t
            ((fun () subj ->
               GT.gmap Std.List.ground
                 ((fun _x__026_ (_x__027_, _x__028_) ->
                    ( (fun () subj -> GT.gmap GT.string subj) _x__026_ _x__027_
                    , (fun () subj -> GT.gmap Scheme.ground subj) _x__026_ _x__028_ ) )
                    () )
                 subj )
               () )
            ((fun () subj -> GT.gmap Scheme.ground subj) ())
            subj
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method show subj = show_ground () subj
  
                method gmap subj = gmap_ground () subj
              end }
  
        let show_ground subj = show_ground () subj
  
        let gmap_ground subj = gmap_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec logic =
        ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
        , Scheme.logic )
        t
        OCanren.logic
      [@@deriving gt ~options:{gmap; show}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit
              [ ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
                , Scheme.logic )
                t
              , ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
                , Scheme.logic )
                t
              , ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
                , Scheme.logic )
                t
              , 'inh
              , 'extra
              , 'syn ]
              OCanren.logic_t
          end
  
        let gcata_logic = OCanren.gcata_logic
  
        class ['extra_logic] show_logic_t _fself_logic =
          object
            inherit [unit, 'extra_logic, string] logic_t
  
            constraint 'extra_logic = logic
  
            inherit
              [ ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
                , Scheme.logic )
                t
              , 'extra_logic ]
              OCanren.show_logic_t
                (fun () subj ->
                  GT.show t
                    ((fun () subj ->
                       GT.show OCanren.Std.List.logic
                         ((fun () subj ->
                            GT.show OCanren.Std.Pair.logic
                              ((fun () subj ->
                                 GT.show OCanren.logic
                                   ((fun () subj -> GT.show GT.string subj) ())
                                   subj )
                                 () )
                              ((fun () subj -> GT.show Scheme.logic subj) ())
                              subj )
                            () )
                         subj )
                       () )
                    ((fun () subj -> GT.show Scheme.logic subj) ())
                    subj )
                _fself_logic
          end
  
        let show_logic () subj =
          GT.show OCanren.logic
            ((fun () subj ->
               GT.show t
                 ((fun () subj ->
                    GT.show OCanren.Std.List.logic
                      ((fun () subj ->
                         GT.show OCanren.Std.Pair.logic
                           ((fun () subj ->
                              GT.show OCanren.logic ((fun () subj -> GT.show GT.string subj) ()) subj
                              )
                              () )
                           ((fun () subj -> GT.show Scheme.logic subj) ())
                           subj )
                         () )
                      subj )
                    () )
                 ((fun () subj -> GT.show Scheme.logic subj) ())
                 subj )
               () )
            subj
  
        class ['extra_logic, 'syn_logic] gmap_logic_t _fself_logic =
          object
            inherit [unit, 'extra_logic, logic] logic_t
  
            constraint 'extra_logic = logic
  
            constraint 'syn_logic = logic
  
            inherit
              [ ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
                , Scheme.logic )
                t
              , ( (GT.string OCanren.logic, Scheme.logic) OCanren.Std.Pair.logic OCanren.Std.List.logic
                , Scheme.logic )
                t
              , 'extra_logic
              , 'syn_logic ]
              OCanren.gmap_logic_t
                (fun () subj ->
                  GT.gmap t
                    ((fun () subj ->
                       GT.gmap OCanren.Std.List.logic
                         ((fun () subj ->
                            GT.gmap OCanren.Std.Pair.logic
                              ((fun () subj ->
                                 GT.gmap OCanren.logic
                                   ((fun () subj -> GT.gmap GT.string subj) ())
                                   subj )
                                 () )
                              ((fun () subj -> GT.gmap Scheme.logic subj) ())
                              subj )
                            () )
                         subj )
                       () )
                    ((fun () subj -> GT.gmap Scheme.logic subj) ())
                    subj )
                _fself_logic
          end
  
        let gmap_logic () subj =
          GT.gmap OCanren.logic
            ((fun () subj ->
               GT.gmap t
                 ((fun () subj ->
                    GT.gmap OCanren.Std.List.logic
                      ((fun () subj ->
                         GT.gmap OCanren.Std.Pair.logic
                           ((fun () subj ->
                              GT.gmap OCanren.logic ((fun () subj -> GT.gmap GT.string subj) ()) subj
                              )
                              () )
                           ((fun () subj -> GT.gmap Scheme.logic subj) ())
                           subj )
                         () )
                      subj )
                    () )
                 ((fun () subj -> GT.gmap Scheme.logic subj) ())
                 subj )
               () )
            subj
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method show subj = show_logic () subj
  
                method gmap subj = gmap_logic () subj
              end }
  
        let show_logic subj = show_logic () subj
  
        let gmap_logic subj = gmap_logic () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec injected =
        ( (GT.string OCanren.ilogic, Scheme.injected) OCanren.Std.Pair.injected
          OCanren.Std.List.injected
        , Scheme.injected )
        t
        OCanren.ilogic
  
      let fmapt f__013_ f__014_ subj__015_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__013_ <*> f__014_ <*> subj__015_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.prj_exn
            <..> chain
                   (fmapt
                      (Std.List.prj_exn (OCanren.Std.Pair.prj_exn OCanren.prj_exn Scheme.prj_exn))
                      Scheme.prj_exn ) )
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework
                         ~fv:
                           (fmapt
                              (Std.List.reify (OCanren.Std.Pair.reify OCanren.reify Scheme.reify))
                              Scheme.reify ) ) ) )
  
      let val_ _x__008_ = OCanren.inji (Val _x__008_)
  
      let closure _x__009_ _x__010_ = OCanren.inji (Closure (_x__009_, _x__010_))
    end
  
    let (_ : injected) = val_ (Scheme.symb !!"a")
  
    let rec inhabit q =
      conde
        [ Fresh.one (fun s -> q === val_ s &&& Scheme.inhabit s)
        ; Fresh.two (fun env f ->
              q === closure env f &&& Scheme.inhabit f
              &&& Scheme.inhabit_list
                    (fun p -> Fresh.two (fun a b -> p === Std.pair a b &&& Scheme.inhabit b))
                    env ) ]
  
    let () =
      print_endline "Inhabitants of scheme interpreter results:" ;
      run q inhabit (fun rr -> rr#reify reify)
      |> Stream.map (GT.show logic)
      |> Stream.take ~n:10
      |> List.iteri (Printf.printf "%2d: %s\n")
  end

  $ ./test007.exe
  test007
  Inhabitants of scheme terms:
   0: Symb (_.11)
   1: Seq ([])
   2: Seq ([Symb (_.16)])
   3: Seq ([Seq ([])])
   4: Seq ([Symb (_.16); Symb (_.23)])
   5: Seq ([Symb (_.16); Seq ([])])
   6: Seq ([Seq ([]); Symb (_.33)])
   7: Seq ([Seq ([Symb (_.26)])])
   8: Seq ([Symb (_.16); Symb (_.23); Symb (_.36)])
   9: Seq ([Seq ([]); Seq ([])])
  Inhabitants of scheme interpreter results:
   0: Val (Symb (_.14))
   1: Val (Seq ([]))
   2: Closure ([], Symb (_.15))
   3: Val (Seq ([Symb (_.22)]))
   4: Closure ([], Seq ([]))
   5: Closure ([(_.25, Symb (_.31))], Symb (_.15))
   6: Val (Seq ([Seq ([])]))
   7: Val (Seq ([Symb (_.22); Symb (_.39)]))
   8: Closure ([(_.25, Seq ([]))], Symb (_.15))
   9: Closure ([(_.42, Symb (_.55))], Seq ([]))
