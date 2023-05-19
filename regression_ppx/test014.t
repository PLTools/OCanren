  $ ../ppx/pp_distrib_gt.exe test014.ml -pretty -new-typenames
  let () = print_endline "test014"
  module _ :
    sig
      type 'a0 aaa_fuly =
        | A2 of 'a0 [@@deriving gt ~options:{ gmap }]
      include
        sig
          class virtual ['ia0,'a0,'sa0,'inh,'extra,'syn] aaa_fuly_t :
            object method  virtual c_A2 : 'inh -> 'extra -> 'a0 -> 'syn end
          val gcata_aaa_fuly :
            < c_A2: 'inh -> 'a0 aaa_fuly -> 'a0 -> 'syn   ;.. >  ->
              'inh -> 'a0 aaa_fuly -> 'syn
          class ['a0,'a0_2,'extra_aaa_fuly,'syn_aaa_fuly] gmap_aaa_fuly_t :
            (unit -> 'a0 -> 'a0_2) ->
              (unit -> 'a0 aaa_fuly -> 'a0_2 aaa_fuly) ->
                object
                  constraint 'extra_aaa_fuly = 'a0 aaa_fuly
                  constraint 'syn_aaa_fuly = 'a0_2 aaa_fuly
                  method  c_A2 :
                    unit -> 'extra_aaa_fuly -> 'a0 -> 'a0_2 aaa_fuly
                end
          val gmap_aaa_fuly : ('a0 -> 'a0_2) -> 'a0 aaa_fuly -> 'a0_2 aaa_fuly
          val aaa_fuly :
            (< c_A2: 'inh -> 'a0 aaa_fuly -> 'a0 -> 'syn   ;.. >  ->
               'inh -> 'a0 aaa_fuly -> 'syn,
              < gmap: ('a0 -> 'a0_2) -> 'a0 aaa_fuly -> 'a0_2 aaa_fuly   > ,
              (('inh2 -> 'a1 aaa_fuly -> 'syn3) ->
                 ('a1_i,'a1,'a1_s,'inh2,'a1 aaa_fuly,'syn3)#aaa_fuly_t)
                -> 'inh2 -> 'a1 aaa_fuly -> 'syn3)
              GT.t
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
      type 'a0 bbb_fuly =
        | B2 of 'a0 [@@deriving gt ~options:{ gmap }]
      include
        sig
          class virtual ['ia0,'a0,'sa0,'inh,'extra,'syn] bbb_fuly_t :
            object method  virtual c_B2 : 'inh -> 'extra -> 'a0 -> 'syn end
          val gcata_bbb_fuly :
            < c_B2: 'inh -> 'a0 bbb_fuly -> 'a0 -> 'syn   ;.. >  ->
              'inh -> 'a0 bbb_fuly -> 'syn
          class ['a0,'a0_2,'extra_bbb_fuly,'syn_bbb_fuly] gmap_bbb_fuly_t :
            (unit -> 'a0 -> 'a0_2) ->
              (unit -> 'a0 bbb_fuly -> 'a0_2 bbb_fuly) ->
                object
                  constraint 'extra_bbb_fuly = 'a0 bbb_fuly
                  constraint 'syn_bbb_fuly = 'a0_2 bbb_fuly
                  method  c_B2 :
                    unit -> 'extra_bbb_fuly -> 'a0 -> 'a0_2 bbb_fuly
                end
          val gmap_bbb_fuly : ('a0 -> 'a0_2) -> 'a0 bbb_fuly -> 'a0_2 bbb_fuly
          val bbb_fuly :
            (< c_B2: 'inh -> 'a0 bbb_fuly -> 'a0 -> 'syn   ;.. >  ->
               'inh -> 'a0 bbb_fuly -> 'syn,
              < gmap: ('a0 -> 'a0_2) -> 'a0 bbb_fuly -> 'a0_2 bbb_fuly   > ,
              (('inh2 -> 'a1 bbb_fuly -> 'syn3) ->
                 ('a1_i,'a1,'a1_s,'inh2,'a1 bbb_fuly,'syn3)#bbb_fuly_t)
                -> 'inh2 -> 'a1 bbb_fuly -> 'syn3)
              GT.t
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
    end =
    struct
      type 'a0 aaa_fuly =
        | A2 of 'a0 [@@deriving gt ~options:{ gmap }]
      include
        struct
          class virtual ['ia0,'a0,'sa0,'inh,'extra,'syn] aaa_fuly_t =
            object method virtual  c_A2 : 'inh -> 'extra -> 'a0 -> 'syn end
          let gcata_aaa_fuly
            (tr : (_,'typ0__002_,_,_,'typ0__002_ aaa_fuly,_)#aaa_fuly_t) inh
            subj = match subj with | A2 _x__001_ -> tr#c_A2 inh subj _x__001_
          class ['a0,'a0_2,'extra_aaa_fuly,'syn_aaa_fuly] gmap_aaa_fuly_t fa0 
            _fself_aaa_fuly =
            object
              inherit  [unit,'a0,'a0_2,unit,'extra_aaa_fuly,'a0_2 aaa_fuly]
                aaa_fuly_t
              constraint 'extra_aaa_fuly = 'a0 aaa_fuly
              constraint 'syn_aaa_fuly = 'a0_2 aaa_fuly
              method c_A2 () _ _x__003_ = A2 (fa0 () _x__003_)
            end
          let gmap_aaa_fuly fa0 inh0 subj =
            GT.transform_gc gcata_aaa_fuly ((new gmap_aaa_fuly_t) fa0) inh0
              subj
          let aaa_fuly =
            {
              GT.gcata = gcata_aaa_fuly;
              GT.fix = (fun eta -> GT.transform_gc gcata_aaa_fuly eta);
              GT.plugins =
                (object
                   method gmap fa0 subj = gmap_aaa_fuly (GT.lift fa0) () subj
                 end)
            }
          let gmap_aaa_fuly fa0 subj = gmap_aaa_fuly (GT.lift fa0) () subj
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
      type 'a0 bbb_fuly =
        | B2 of 'a0 [@@deriving gt ~options:{ gmap }]
      include
        struct
          class virtual ['ia0,'a0,'sa0,'inh,'extra,'syn] bbb_fuly_t =
            object method virtual  c_B2 : 'inh -> 'extra -> 'a0 -> 'syn end
          let gcata_bbb_fuly
            (tr : (_,'typ0__005_,_,_,'typ0__005_ bbb_fuly,_)#bbb_fuly_t) inh
            subj = match subj with | B2 _x__004_ -> tr#c_B2 inh subj _x__004_
          class ['a0,'a0_2,'extra_bbb_fuly,'syn_bbb_fuly] gmap_bbb_fuly_t fa0 
            _fself_bbb_fuly =
            object
              inherit  [unit,'a0,'a0_2,unit,'extra_bbb_fuly,'a0_2 bbb_fuly]
                bbb_fuly_t
              constraint 'extra_bbb_fuly = 'a0 bbb_fuly
              constraint 'syn_bbb_fuly = 'a0_2 bbb_fuly
              method c_B2 () _ _x__006_ = B2 (fa0 () _x__006_)
            end
          let gmap_bbb_fuly fa0 inh0 subj =
            GT.transform_gc gcata_bbb_fuly ((new gmap_bbb_fuly_t) fa0) inh0
              subj
          let bbb_fuly =
            {
              GT.gcata = gcata_bbb_fuly;
              GT.fix = (fun eta -> GT.transform_gc gcata_bbb_fuly eta);
              GT.plugins =
                (object
                   method gmap fa0 subj = gmap_bbb_fuly (GT.lift fa0) () subj
                 end)
            }
          let gmap_bbb_fuly fa0 subj = gmap_bbb_fuly (GT.lift fa0) () subj
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
      type 'a aaa = ('a * 'a bbb GT.list)
      and 'a bbb = 'a aaa GT.list[@@deriving gt ~options:{ gmap }]
      include
        struct
          class virtual ['ia,'a,'sa,'inh,'extra,'syn] aaa_t =
            object method virtual  c_AAA : 'inh -> 'extra -> 'syn end
          class virtual ['ia,'a,'sa,'inh,'extra,'syn] bbb_t =
            object inherit  ['ia aaa,'a aaa,'sa aaa,'inh,'extra,'syn] GT.list_t
            end
          let gcata_aaa (tr : (_,'typ0__007_,_,_,'typ0__007_ aaa,_)#aaa_t) inh
            subj = tr#c_AAA inh subj
          let gcata_bbb = GT.gcata_list
          let fix_aaa aaa0 bbb0 =
            let rec traitaaa fa inh subj =
              gcata_aaa (aaa0 (traitaaa, traitbbb) fa) inh subj
            and traitbbb fa inh subj =
              gcata_bbb (bbb0 (traitaaa, traitbbb) fa) inh subj in
            (traitaaa, traitbbb)
          class ['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t_stub ((_fself_aaa,
                                                                gmap_bbb) as
                                                                 _mutuals_pack)
             fa =
            object
              inherit  [unit,'a,'a_2,unit,'extra_aaa,'a_2 aaa] aaa_t
              constraint 'extra_aaa = 'a aaa
              constraint 'syn_aaa = 'a_2 aaa
              method c_AAA () (_x__010_, _x__011_) =
                ((fa () _x__010_),
                  ((fun () -> fun subj -> GT.gmap GT.list (gmap_bbb fa ()) subj)
                     () _x__011_))
            end
          class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t_stub ((gmap_aaa,
                                                                _fself_bbb) as
                                                                 _mutuals_pack)
             fa =
            object
              inherit  [unit,'a,'a_2,unit,'extra_bbb,'a_2 bbb] bbb_t
              constraint 'extra_bbb = 'a bbb
              constraint 'syn_bbb = 'a_2 bbb
              inherit  ((['a aaa,'a_2 aaa,'extra_bbb,'syn_bbb] GT.gmap_list_t)
                (gmap_aaa fa) (_fself_bbb fa))
            end
          let gmap_aaa_0 = new gmap_aaa_t_stub
          let gmap_bbb_0 = new gmap_bbb_t_stub
          let gmap_aaa eta__008_ =
            let (f, _) = fix_aaa gmap_aaa_0 gmap_bbb_0 in f eta__008_
          let gmap_bbb eta__009_ =
            let (_, f) = fix_aaa gmap_aaa_0 gmap_bbb_0 in f eta__009_
          class ['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t _  fa =
            object
              inherit  ((['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t_stub)
                (gmap_aaa, gmap_bbb) fa)
            end
          class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t _  fa =
            object
              inherit  ((['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t_stub)
                (gmap_aaa, gmap_bbb) fa)
            end
          let aaa =
            {
              GT.gcata = gcata_aaa;
              GT.fix = fix_aaa;
              GT.plugins =
                (object method gmap fa subj = gmap_aaa (GT.lift fa) () subj end)
            }
          let gmap_aaa fa subj = gmap_aaa (GT.lift fa) () subj
          let bbb =
            {
              GT.gcata = gcata_bbb;
              GT.fix = fix_aaa;
              GT.plugins =
                (object method gmap fa subj = gmap_bbb (GT.lift fa) () subj end)
            }
          let gmap_bbb fa subj = gmap_bbb (GT.lift fa) () subj
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
      let __ (type a) (type b) = (GT.gmap aaa : (a -> b) -> a aaa -> b aaa)
      let __ (type a) (type b) = (GT.gmap bbb : (a -> b) -> a bbb -> b bbb)
    end 
  $ ./test014.exe
  test014
