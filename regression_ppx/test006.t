  $ ../ppx/pp_gt.exe -pp pp_ocanren_all test006.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  open OCanren
  
  module Gterm = struct
    include struct
      type nonrec ('s, 'xs) t = Symb of 's | Seq of 'xs [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['is, 's, 'ss, 'ixs, 'xs, 'sxs, 'inh, 'extra, 'syn] t_t =
          object
            method virtual c_Symb : 'inh -> 'extra -> 's -> 'syn
  
            method virtual c_Seq : 'inh -> 'extra -> 'xs -> 'syn
          end
  
        let gcata_t
            (tr : (_, 'typ0__003_, _, _, 'typ1__004_, _, _, ('typ0__003_, 'typ1__004_) t, _) #t_t) inh
            subj =
          match subj with
          | Symb _x__001_ ->
              tr#c_Symb inh subj _x__001_
          | Seq _x__002_ ->
              tr#c_Seq inh subj _x__002_
  
        class ['s, 's_2, 'xs, 'xs_2, 'extra_t, 'syn_t] gmap_t_t fs fxs =
          let _ = fs in
          fun _fself_t ->
            object
              inherit [unit, 's, 's_2, unit, 'xs, 'xs_2, unit, 'extra_t, ('s_2, 'xs_2) t] t_t
  
              constraint 'extra_t = ('s, 'xs) t
  
              constraint 'syn_t = ('s_2, 'xs_2) t
  
              method c_Symb () _ _x__005_ = Symb (fs () _x__005_)
  
              method c_Seq () _ _x__006_ = Seq (fxs () _x__006_)
            end
  
        let gmap_t fs fxs =
          let _ = fs in
          fun inh0 subj -> GT.transform_gc gcata_t (new gmap_t_t fs fxs) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method gmap fs fxs subj = gmap_t (GT.lift fs) (GT.lift fxs) () subj
              end }
  
        let gmap_t fs fxs subj = gmap_t (GT.lift fs) (GT.lift fxs) () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type ground = (GT.string, ground Std.List.ground) t [@@deriving gt ~options:{gmap}]
  
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
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_ground () subj
              end }
  
        let gmap_ground subj = gmap_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type logic = (GT.string OCanren.logic, logic OCanren.Std.List.logic) t OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
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
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_logic () subj
              end }
  
        let gmap_logic subj = gmap_logic () subj
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
  end
  
  module Gresult = struct
    include struct
      type nonrec ('s, 't, 'xs) t = Closure of 's * 't * 'xs | Val of 't
      [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['is, 's, 'ss, 'it, 't, 'st, 'ixs, 'xs, 'sxs, 'inh, 'extra, 'syn] t_t =
          object
            method virtual c_Closure : 'inh -> 'extra -> 's -> 't -> 'xs -> 'syn
  
            method virtual c_Val : 'inh -> 'extra -> 't -> 'syn
          end
  
        let gcata_t
            (tr :
              ( _
              , 'typ0__011_
              , _
              , _
              , 'typ1__012_
              , _
              , _
              , 'typ2__013_
              , _
              , _
              , ('typ0__011_, 'typ1__012_, 'typ2__013_) t
              , _ )
              #t_t ) inh subj =
          match subj with
          | Closure (_x__007_, _x__008_, _x__009_) ->
              tr#c_Closure inh subj _x__007_ _x__008_ _x__009_
          | Val _x__010_ ->
              tr#c_Val inh subj _x__010_
  
        class ['s, 's_2, 't, 't_2, 'xs, 'xs_2, 'extra_t, 'syn_t] gmap_t_t fs ft fxs =
          let _ = fxs in
          let _ = fs in
          fun _fself_t ->
            object
              inherit
                [ unit
                , 's
                , 's_2
                , unit
                , 't
                , 't_2
                , unit
                , 'xs
                , 'xs_2
                , unit
                , 'extra_t
                , ('s_2, 't_2, 'xs_2) t ]
                t_t
  
              constraint 'extra_t = ('s, 't, 'xs) t
  
              constraint 'syn_t = ('s_2, 't_2, 'xs_2) t
  
              method c_Closure () _ _x__014_ _x__015_ _x__016_ =
                Closure (fs () _x__014_, ft () _x__015_, fxs () _x__016_)
  
              method c_Val () _ _x__017_ = Val (ft () _x__017_)
            end
  
        let gmap_t fs ft fxs =
          let _ = fxs in
          let _ = fs in
          fun inh0 subj -> GT.transform_gc gcata_t (new gmap_t_t fs ft fxs) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method gmap fs ft fxs subj = gmap_t (GT.lift fs) (GT.lift ft) (GT.lift fxs) () subj
              end }
  
        let gmap_t fs ft fxs subj = gmap_t (GT.lift fs) (GT.lift ft) (GT.lift fxs) () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type ground = (GT.string, Gterm.ground, (GT.string, ground) Std.Pair.ground Std.List.ground) t
      [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] ground_t =
          object
            inherit
              [ GT.string
              , GT.string
              , GT.string
              , Gterm.ground
              , Gterm.ground
              , Gterm.ground
              , (GT.string, ground) Std.Pair.ground Std.List.ground
              , (GT.string, ground) Std.Pair.ground Std.List.ground
              , (GT.string, ground) Std.Pair.ground Std.List.ground
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
              , Gterm.ground
              , Gterm.ground
              , (GT.string, ground) Std.Pair.ground Std.List.ground
              , (GT.string, ground) Std.Pair.ground Std.List.ground
              , 'extra_ground
              , 'syn_ground ]
              gmap_t_t
                (fun () subj -> GT.gmap GT.string subj)
                (fun () subj -> GT.gmap Gterm.ground subj)
                (fun () subj ->
                  GT.gmap Std.List.ground
                    ((fun () subj ->
                       GT.gmap Std.Pair.ground
                         ((fun () subj -> GT.gmap GT.string subj) ())
                         (_fself_ground ()) subj )
                       () )
                    subj )
                _fself_ground
          end
  
        let rec gmap_ground () subj =
          GT.gmap t
            ((fun () subj -> GT.gmap GT.string subj) ())
            ((fun () subj -> GT.gmap Gterm.ground subj) ())
            ((fun () subj ->
               GT.gmap Std.List.ground
                 ((fun () subj ->
                    GT.gmap Std.Pair.ground
                      ((fun () subj -> GT.gmap GT.string subj) ())
                      (gmap_ground ()) subj )
                    () )
                 subj )
               () )
            subj
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_ground () subj
              end }
  
        let gmap_ground subj = gmap_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type logic =
        ( GT.string OCanren.logic
        , Gterm.logic
        , (GT.string OCanren.logic, logic) Std.Pair.logic OCanren.Std.List.logic )
        t
        OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit
              [ ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic OCanren.Std.List.logic )
                t
              , ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic OCanren.Std.List.logic )
                t
              , ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic OCanren.Std.List.logic )
                t
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
              [ ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic OCanren.Std.List.logic )
                t
              , ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic OCanren.Std.List.logic )
                t
              , 'extra_logic
              , 'syn_logic ]
              OCanren.gmap_logic_t
                (fun () subj ->
                  GT.gmap t
                    ((fun () subj ->
                       GT.gmap OCanren.logic ((fun () subj -> GT.gmap GT.string subj) ()) subj )
                       () )
                    ((fun () subj -> GT.gmap Gterm.logic subj) ())
                    ((fun () subj ->
                       GT.gmap OCanren.Std.List.logic
                         ((fun () subj ->
                            GT.gmap Std.Pair.logic
                              ((fun () subj ->
                                 GT.gmap OCanren.logic
                                   ((fun () subj -> GT.gmap GT.string subj) ())
                                   subj )
                                 () )
                              (_fself_logic ()) subj )
                            () )
                         subj )
                       () )
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
                 ((fun () subj -> GT.gmap Gterm.logic subj) ())
                 ((fun () subj ->
                    GT.gmap OCanren.Std.List.logic
                      ((fun () subj ->
                         GT.gmap Std.Pair.logic
                           ((fun () subj ->
                              GT.gmap OCanren.logic ((fun () subj -> GT.gmap GT.string subj) ()) subj
                              )
                              () )
                           (gmap_logic ()) subj )
                         () )
                      subj )
                    () )
                 subj )
               () )
            subj
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_logic () subj
              end }
  
        let gmap_logic subj = gmap_logic () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type injected =
        ( GT.string OCanren.ilogic
        , Gterm.injected
        , (GT.string OCanren.ilogic, injected) Std.Pair.injected OCanren.Std.List.injected )
        t
        OCanren.ilogic
  
      let fmapt f__015_ f__016_ f__017_ subj__018_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__015_ <*> f__016_ <*> f__017_ <*> subj__018_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.prj_exn
            <..> chain
                   (fmapt OCanren.prj_exn Gterm.prj_exn
                      (Std.List.prj_exn (Std.Pair.prj_exn OCanren.prj_exn self)) ) )
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
            OCanren.reify
            <..> chain
                   (OCanren.Reifier.zed
                      (OCanren.Reifier.rework
                         ~fv:
                           (fmapt OCanren.reify Gterm.reify
                              (Std.List.reify (Std.Pair.reify OCanren.reify self)) ) ) ) )
  
      let closure _x__008_ _x__009_ _x__010_ = OCanren.inji (Closure (_x__008_, _x__009_, _x__010_))
  
      let val_ _x__011_ = OCanren.inji (Val _x__011_)
    end
  end
  
  module _ = struct
    include struct
      type nonrec t = T of {x: GT.int; y: GT.int} [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] t_t =
          object
            method virtual c_T : 'inh -> 'extra -> GT.int -> GT.int -> 'syn
          end
  
        let gcata_t (tr : (_, t, _) #t_t) inh subj =
          match subj with T {x= _x__018_; y= _x__019_} -> tr#c_T inh subj _x__018_ _x__019_
  
        class ['extra_t, 'syn_t] gmap_t_t _fself_t =
          object
            inherit [unit, 'extra_t, t] t_t
  
            constraint 'extra_t = t
  
            constraint 'syn_t = t
  
            method c_T () _ _x__020_ _x__021_ =
              T
                { x= (fun () subj -> GT.gmap GT.int subj) () _x__020_
                ; y= (fun () subj -> GT.gmap GT.int subj) () _x__021_ }
          end
  
        let gmap_t inh0 subj = GT.transform_gc gcata_t (new gmap_t_t) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_t () subj
              end }
  
        let gmap_t subj = gmap_t () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec ground = t [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] ground_t =
          object
            inherit ['inh, 'extra, 'syn] t_t
          end
  
        let gcata_ground = gcata_t
  
        class ['extra_ground, 'syn_ground] gmap_ground_t _fself_ground =
          object
            inherit [unit, 'extra_ground, ground] ground_t
  
            constraint 'extra_ground = ground
  
            constraint 'syn_ground = ground
  
            inherit ['extra_ground, 'syn_ground] gmap_t_t _fself_ground
          end
  
        let gmap_ground () subj = GT.gmap t subj
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_ground () subj
              end }
  
        let gmap_ground subj = gmap_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec logic = t OCanren.logic [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit [t, t, t, 'inh, 'extra, 'syn] OCanren.logic_t
          end
  
        let gcata_logic = OCanren.gcata_logic
  
        class ['extra_logic, 'syn_logic] gmap_logic_t _fself_logic =
          object
            inherit [unit, 'extra_logic, logic] logic_t
  
            constraint 'extra_logic = logic
  
            constraint 'syn_logic = logic
  
            inherit
              [t, t, 'extra_logic, 'syn_logic] OCanren.gmap_logic_t
                (fun () subj -> GT.gmap t subj)
                _fself_logic
          end
  
        let gmap_logic () subj = GT.gmap OCanren.logic ((fun () subj -> GT.gmap t subj) ()) subj
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_logic () subj
              end }
  
        let gmap_logic subj = gmap_logic () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec injected = t OCanren.ilogic
  
      let fmapt subj__019_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> subj__019_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain fmapt)
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:fmapt)) )
  
      let t x y = OCanren.inji (T {x; y})
    end
  end
  
  module _ = struct
    include struct
      type nonrec t = {x: GT.int; y: GT.int} [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] t_t =
          object
            method virtual do_t : 'inh -> t -> 'syn
          end
  
        let gcata_t (tr : (_, t, _) #t_t) inh subj = tr#do_t inh subj
  
        class ['extra_t, 'syn_t] gmap_t_t _fself_t =
          object
            inherit [unit, 'extra_t, t] t_t
  
            constraint 'extra_t = t
  
            constraint 'syn_t = t
  
            method do_t () {x; y} =
              { x= (fun () subj -> GT.gmap GT.int subj) () x
              ; y= (fun () subj -> GT.gmap GT.int subj) () y }
          end
  
        let gmap_t inh0 subj = GT.transform_gc gcata_t (new gmap_t_t) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_t () subj
              end }
  
        let gmap_t subj = gmap_t () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec ground = t [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] ground_t =
          object
            inherit ['inh, 'extra, 'syn] t_t
          end
  
        let gcata_ground = gcata_t
  
        class ['extra_ground, 'syn_ground] gmap_ground_t _fself_ground =
          object
            inherit [unit, 'extra_ground, ground] ground_t
  
            constraint 'extra_ground = ground
  
            constraint 'syn_ground = ground
  
            inherit ['extra_ground, 'syn_ground] gmap_t_t _fself_ground
          end
  
        let gmap_ground () subj = GT.gmap t subj
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_ground () subj
              end }
  
        let gmap_ground subj = gmap_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec logic = t OCanren.logic [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit [t, t, t, 'inh, 'extra, 'syn] OCanren.logic_t
          end
  
        let gcata_logic = OCanren.gcata_logic
  
        class ['extra_logic, 'syn_logic] gmap_logic_t _fself_logic =
          object
            inherit [unit, 'extra_logic, logic] logic_t
  
            constraint 'extra_logic = logic
  
            constraint 'syn_logic = logic
  
            inherit
              [t, t, 'extra_logic, 'syn_logic] OCanren.gmap_logic_t
                (fun () subj -> GT.gmap t subj)
                _fself_logic
          end
  
        let gmap_logic () subj = GT.gmap OCanren.logic ((fun () subj -> GT.gmap t subj) ()) subj
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_logic () subj
              end }
  
        let gmap_logic subj = gmap_logic () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type nonrec injected = t OCanren.ilogic
  
      let fmapt subj__020_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> subj__020_
  
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ -> OCanren.prj_exn <..> chain fmapt)
  
      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun _ ->
            OCanren.reify <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:fmapt)) )
  
      let make_t x y = OCanren.inji {x; y}
    end
  end
  
  let () = print_endline "test006"
  
  module _ = struct
    include struct
      type nonrec ('a1, 'a0) t = Symb of 'a1 | Seq of 'a0 [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['ia1, 'a1, 'sa1, 'ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] t_t =
          object
            method virtual c_Symb : 'inh -> 'extra -> 'a1 -> 'syn
  
            method virtual c_Seq : 'inh -> 'extra -> 'a0 -> 'syn
          end
  
        let gcata_t
            (tr : (_, 'typ0__024_, _, _, 'typ1__025_, _, _, ('typ0__024_, 'typ1__025_) t, _) #t_t) inh
            subj =
          match subj with
          | Symb _x__022_ ->
              tr#c_Symb inh subj _x__022_
          | Seq _x__023_ ->
              tr#c_Seq inh subj _x__023_
  
        class ['a1, 'a1_2, 'a0, 'a0_2, 'extra_t, 'syn_t] gmap_t_t fa1 fa0 =
          let _ = fa1 in
          fun _fself_t ->
            object
              inherit [unit, 'a1, 'a1_2, unit, 'a0, 'a0_2, unit, 'extra_t, ('a1_2, 'a0_2) t] t_t
  
              constraint 'extra_t = ('a1, 'a0) t
  
              constraint 'syn_t = ('a1_2, 'a0_2) t
  
              method c_Symb () _ _x__026_ = Symb (fa1 () _x__026_)
  
              method c_Seq () _ _x__027_ = Seq (fa0 () _x__027_)
            end
  
        let gmap_t fa1 fa0 =
          let _ = fa1 in
          fun inh0 subj -> GT.transform_gc gcata_t (new gmap_t_t fa1 fa0) inh0 subj
  
        let t =
          { GT.gcata= gcata_t
          ; GT.fix= (fun eta -> GT.transform_gc gcata_t eta)
          ; GT.plugins=
              object
                method gmap fa1 fa0 subj = gmap_t (GT.lift fa1) (GT.lift fa0) () subj
              end }
  
        let gmap_t fa1 fa0 subj = gmap_t (GT.lift fa1) (GT.lift fa0) () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type ground = (GT.string, ground Std.List.ground) t [@@deriving gt ~options:{gmap}]
  
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
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_ground () subj
              end }
  
        let gmap_ground subj = gmap_ground () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type logic = (GT.string OCanren.logic, logic OCanren.Std.List.logic) t OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
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
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins=
              object
                method gmap subj = gmap_logic () subj
              end }
  
        let gmap_logic subj = gmap_logic () subj
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type injected = (GT.string OCanren.ilogic, injected OCanren.Std.List.injected) t OCanren.ilogic
  
      let fmapt f__025_ f__026_ subj__027_ =
        let open OCanren.Env.Monad in
        OCanren.Env.Monad.return (GT.gmap t) <*> f__025_ <*> f__026_ <*> subj__027_
  
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
  
      let symb _x__021_ = OCanren.inji (Symb _x__021_)
  
      let seq _x__022_ = OCanren.inji (Seq _x__022_)
    end
  end
  
  module _ = struct
    include struct
      type ground = GT.bool * GT.int * GT.string
  
      type logic =
        (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
  
      type injected =
        (GT.bool OCanren.ilogic * GT.int OCanren.ilogic * GT.string OCanren.ilogic) OCanren.ilogic
  
      let (reify :
            ( _
            , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
            )
            OCanren.Reifier.t ) =
        (fun r__035_ r__036_ r__037_ ->
          let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
          let fmapt f__038_ f__039_ f__040_ subj__041_ =
            let open OCanren.Env.Monad in
            OCanren.Env.Monad.return gmap_tuple <*> f__038_ <*> f__039_ <*> f__040_ <*> subj__041_
          in
          OCanren.Reifier.fix (fun _ ->
              let open OCanren.Env.Monad in
              OCanren.reify
              <..> chain
                     (OCanren.Reifier.zed
                        (OCanren.Reifier.rework ~fv:(fmapt r__035_ r__036_ r__037_)) ) ) )
          OCanren.reify OCanren.reify OCanren.reify
  
      let (prj_exn : (_, GT.bool * GT.int * GT.string) OCanren.Reifier.t) =
        (fun r__028_ r__029_ r__030_ ->
          let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
          let fmapt f__031_ f__032_ f__033_ subj__034_ =
            let open OCanren.Env.Monad in
            OCanren.Env.Monad.return gmap_tuple <*> f__031_ <*> f__032_ <*> f__033_ <*> subj__034_
          in
          OCanren.Reifier.fix (fun _ ->
              let open OCanren.Env.Monad in
              OCanren.prj_exn <..> chain (fmapt r__028_ r__029_ r__030_) ) )
          OCanren.prj_exn OCanren.prj_exn OCanren.prj_exn
    end
  
    let () =
      (let open OCanren in
       run q )
        (fun q -> q === inj (!!true, !!5, !!"x"))
        (fun rr -> rr#reify prj_exn)
      |> Stream.iter (fun (b, n, s) -> Printf.printf "%b %d %S\n" b n s)
  end
  
  module _ = struct
    include struct
      type ground = (GT.bool * GT.int * GT.string) * (GT.bool * GT.int * GT.string) [@@deriving gt]
  
      include struct
        class virtual ['inh, 'extra, 'syn] ground_t =
          object
            method virtual c_GROUND : 'inh -> 'extra -> 'syn
          end
  
        let gcata_ground (tr : (_, ground, _) #ground_t) inh subj = tr#c_GROUND inh subj
  
        let ground =
          { GT.gcata= gcata_ground
          ; GT.fix= (fun eta -> GT.transform_gc gcata_ground eta)
          ; GT.plugins= object end }
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type logic =
        ( (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
        , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic )
        OCanren.Std.Pair.logic
      [@@deriving gt]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit
              [ (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              , (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              , 'inh
              , 'extra
              , 'syn ]
              OCanren.Std.Pair.logic_t
          end
  
        let gcata_logic = OCanren.Std.Pair.gcata_logic
  
        let logic =
          { GT.gcata= gcata_logic
          ; GT.fix= (fun eta -> GT.transform_gc gcata_logic eta)
          ; GT.plugins= object end }
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
      type injected =
        ( (GT.bool OCanren.ilogic * GT.int OCanren.ilogic * GT.string OCanren.ilogic) OCanren.ilogic
        , (GT.bool OCanren.ilogic * GT.int OCanren.ilogic * GT.string OCanren.ilogic) OCanren.ilogic
        )
        OCanren.Std.Pair.injected
  
      let (reify :
            ( _
            , ( (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              * (GT.bool OCanren.logic * GT.int OCanren.logic * GT.string OCanren.logic) OCanren.logic
              )
              OCanren.logic )
            OCanren.Reifier.t ) =
        OCanren.Std.Pair.reify
          ((fun r__063_ r__064_ r__065_ ->
             let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
             let fmapt f__066_ f__067_ f__068_ subj__069_ =
               let open OCanren.Env.Monad in
               OCanren.Env.Monad.return gmap_tuple <*> f__066_ <*> f__067_ <*> f__068_ <*> subj__069_
             in
             OCanren.Reifier.fix (fun _ ->
                 let open OCanren.Env.Monad in
                 OCanren.reify
                 <..> chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(fmapt r__063_ r__064_ r__065_)) ) ) )
             OCanren.reify OCanren.reify OCanren.reify )
          ((fun r__056_ r__057_ r__058_ ->
             let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
             let fmapt f__059_ f__060_ f__061_ subj__062_ =
               let open OCanren.Env.Monad in
               OCanren.Env.Monad.return gmap_tuple <*> f__059_ <*> f__060_ <*> f__061_ <*> subj__062_
             in
             OCanren.Reifier.fix (fun _ ->
                 let open OCanren.Env.Monad in
                 OCanren.reify
                 <..> chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(fmapt r__056_ r__057_ r__058_)) ) ) )
             OCanren.reify OCanren.reify OCanren.reify )
  
      let (prj_exn :
            (_, (GT.bool * GT.int * GT.string) * (GT.bool * GT.int * GT.string)) OCanren.Reifier.t ) =
        OCanren.Std.Pair.prj_exn
          ((fun r__049_ r__050_ r__051_ ->
             let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
             let fmapt f__052_ f__053_ f__054_ subj__055_ =
               let open OCanren.Env.Monad in
               OCanren.Env.Monad.return gmap_tuple <*> f__052_ <*> f__053_ <*> f__054_ <*> subj__055_
             in
             OCanren.Reifier.fix (fun _ ->
                 let open OCanren.Env.Monad in
                 OCanren.prj_exn <..> chain (fmapt r__049_ r__050_ r__051_) ) )
             OCanren.prj_exn OCanren.prj_exn OCanren.prj_exn )
          ((fun r__042_ r__043_ r__044_ ->
             let gmap_tuple f0 f1 f2 (f0s, f1s, f2s) = (f0 f0s, f1 f1s, f2 f2s) in
             let fmapt f__045_ f__046_ f__047_ subj__048_ =
               let open OCanren.Env.Monad in
               OCanren.Env.Monad.return gmap_tuple <*> f__045_ <*> f__046_ <*> f__047_ <*> subj__048_
             in
             OCanren.Reifier.fix (fun _ ->
                 let open OCanren.Env.Monad in
                 OCanren.prj_exn <..> chain (fmapt r__042_ r__043_ r__044_) ) )
             OCanren.prj_exn OCanren.prj_exn OCanren.prj_exn )
    end
  end

  $ ./test006.exe
  test006
  true 5 "x"
