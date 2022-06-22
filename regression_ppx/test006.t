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
  
      type logic = (GT.string OCanren.logic, logic Std.List.logic) t OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit
              [ (GT.string OCanren.logic, logic Std.List.logic) t
              , (GT.string OCanren.logic, logic Std.List.logic) t
              , (GT.string OCanren.logic, logic Std.List.logic) t
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
              [ (GT.string OCanren.logic, logic Std.List.logic) t
              , (GT.string OCanren.logic, logic Std.List.logic) t
              , 'extra_logic
              , 'syn_logic ]
              OCanren.gmap_logic_t
                (fun () subj ->
                  GT.gmap t
                    ((fun () subj ->
                       GT.gmap OCanren.logic ((fun () subj -> GT.gmap GT.string subj) ()) subj )
                       () )
                    ((fun () subj -> GT.gmap Std.List.logic (_fself_logic ()) subj) ())
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
                 ((fun () subj -> GT.gmap Std.List.logic (gmap_logic ()) subj) ())
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
  
      type injected = (GT.string OCanren.ilogic, injected Std.List.injected) t OCanren.ilogic
  
      let fmapt s__003_ xs__004_ subj__005_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> s__003_ <*> xs__004_ <*> subj__005_
  
      let (prj_exn : (_, (GT.string, ground Std.List.ground) t) Reifier.t) =
        let open Env.Monad in
        Reifier.fix (fun self ->
            OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn (Std.List.prj_exn self)) )
  
      let (reify : (_, (GT.string OCanren.logic, logic Std.List.logic) t OCanren.logic) Reifier.t) =
        let open Env.Monad in
        Reifier.fix (fun self ->
            OCanren.reify
            <..> chain (Reifier.zed (Reifier.rework ~fv:(fmapt OCanren.reify (Std.List.reify self)))) )
  
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
        , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
        t
        OCanren.logic
      [@@deriving gt ~options:{gmap}]
  
      include struct
        class virtual ['inh, 'extra, 'syn] logic_t =
          object
            inherit
              [ ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
                t
              , ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
                t
              , ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
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
                , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
                t
              , ( GT.string OCanren.logic
                , Gterm.logic
                , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
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
                       GT.gmap Std.List.logic
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
                    GT.gmap Std.List.logic
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
        , (GT.string OCanren.ilogic, injected) Std.Pair.injected Std.List.injected )
        t
        OCanren.ilogic
  
      let fmapt s__010_ t__011_ xs__012_ subj__013_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> s__010_ <*> t__011_ <*> xs__012_ <*> subj__013_
  
      let (prj_exn :
            ( _
            , (GT.string, Gterm.ground, (GT.string, ground) Std.Pair.ground Std.List.ground) t )
            Reifier.t ) =
        let open Env.Monad in
        Reifier.fix (fun self ->
            OCanren.prj_exn
            <..> chain
                   (fmapt OCanren.prj_exn Gterm.prj_exn
                      (Std.List.prj_exn (Std.Pair.prj_exn OCanren.prj_exn self)) ) )
  
      let (reify :
            ( _
            , ( GT.string OCanren.logic
              , Gterm.logic
              , (GT.string OCanren.logic, logic) Std.Pair.logic Std.List.logic )
              t
              OCanren.logic )
            Reifier.t ) =
        let open Env.Monad in
        Reifier.fix (fun self ->
            OCanren.reify
            <..> chain
                   (Reifier.zed
                      (Reifier.rework
                         ~fv:
                           (fmapt OCanren.reify Gterm.reify
                              (Std.List.reify (Std.Pair.reify OCanren.reify self)) ) ) ) )
  
      let closure _x__006_ _x__007_ _x__008_ = OCanren.inji (Closure (_x__006_, _x__007_, _x__008_))
  
      let val_ _x__009_ = OCanren.inji (Val _x__009_)
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
  
      type ground = t [@@deriving gt ~options:{gmap}]
  
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
  
      type logic = t OCanren.logic [@@deriving gt ~options:{gmap}]
  
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
  
      type injected = t OCanren.ilogic
  
      let fmapt subj__014_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> subj__014_
  
      let (prj_exn : (_, t) Reifier.t) =
        let open Env.Monad in
        Reifier.fix (fun self -> OCanren.prj_exn <..> chain fmapt)
  
      let (reify : (_, t OCanren.logic) Reifier.t) =
        let open Env.Monad in
        Reifier.fix (fun self -> OCanren.reify <..> chain (Reifier.zed (Reifier.rework ~fv:fmapt)))
  
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
  
      type ground = t [@@deriving gt ~options:{gmap}]
  
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
  
      type logic = t OCanren.logic [@@deriving gt ~options:{gmap}]
  
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
  
      type injected = t OCanren.ilogic
  
      let fmapt subj__015_ =
        let open Env.Monad in
        Env.Monad.return (GT.gmap t) <*> subj__015_
  
      let (prj_exn : (_, t) Reifier.t) =
        let open Env.Monad in
        Reifier.fix (fun self -> OCanren.prj_exn <..> chain fmapt)
  
      let (reify : (_, t OCanren.logic) Reifier.t) =
        let open Env.Monad in
        Reifier.fix (fun self -> OCanren.reify <..> chain (Reifier.zed (Reifier.rework ~fv:fmapt)))
  
      let t x y = OCanren.inji {x; y}
    end
  end
  
  let () = print_endline "test006"

  $ ./test006.exe
  test006
