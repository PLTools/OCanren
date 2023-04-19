  $ ../ppx/pp_ocanren_all.exe test013mutual.ml -pretty -new-typenames
  let () = print_endline "test012"
  include
    struct
      type nonrec polarity_fuly =
        | Extends 
        | Super [@@deriving gt ~options:{ gmap; fmt }]
      type nonrec polarity = polarity_fuly[@@deriving
                                            gt ~options:{ gmap; fmt }]
      type nonrec polarity_logic = polarity_fuly OCanren.logic[@@deriving
                                                                gt
                                                                  ~options:
                                                                  { gmap; fmt
                                                                  }]
      type nonrec polarity_injected = polarity_fuly OCanren.ilogic
      let polarity_fmapt subj__001_ =
        let open OCanren.Env.Monad in
          (OCanren.Env.Monad.return (GT.gmap polarity_fuly)) <*> subj__001_
      let (polarity_prj_exn : (polarity_injected, polarity) OCanren.Reifier.t)
        =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun _ -> OCanren.prj_exn <..> (chain polarity_fmapt))
      let (polarity_reify :
        (polarity_injected, polarity_logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun _ ->
               OCanren.reify <..>
                 (chain
                    (OCanren.Reifier.zed
                       (OCanren.Reifier.rework ~fv:polarity_fmapt))))
    end
  include
    struct
      type nonrec id = GT.int[@@deriving gt ~options:{ gmap; fmt }]
      type nonrec id_logic = GT.int OCanren.logic[@@deriving
                                                   gt ~options:{ gmap; fmt }]
      type nonrec id_injected = GT.int OCanren.ilogic
      let (id_reify : (_, GT.int OCanren.logic) OCanren.Reifier.t) =
        OCanren.reify
      let (id_prj_exn : (_, GT.int) OCanren.Reifier.t) = OCanren.prj_exn
    end
  include
    struct
      type ('a1, 'a0) targ_fuly =
        | T of 'a1 
        | Wildcard of 'a0 
      and ('a6, 'a5, 'a4, 'a3, 'a2, 'a1, 'a0) jtype_fuly =
        | Array of 'a0 
        | Class of 'a4 * 'a6 
        | Interface of 'a4 * 'a5 
        | V of {
        id: 'a4 ;
        index: 'a3 ;
        upb: 'a2 ;
        lwb: 'a1 } 
        | Null 
        | Intersect of 'a0 [@@deriving gt ~options:{ gmap }]
      type targ = (jtype, (polarity * jtype) GT.option) targ_fuly
      and jtype =
        (targ OCanren.Std.List.ground, targ, id, int, jtype, jtype GT.option,
          jtype OCanren.Std.List.ground) jtype_fuly
      type targ_logic =
        (jtype_logic,
          (polarity_logic, jtype_logic) OCanren.Std.Pair.logic GT.option
            OCanren.logic)
          targ_fuly OCanren.logic
      and jtype_logic =
        (targ_logic OCanren.Std.List.logic, targ_logic, id_logic,
          int OCanren.logic, jtype_logic, jtype_logic GT.option OCanren.logic,
          jtype_logic OCanren.Std.List.logic) jtype_fuly OCanren.logic
      type targ_injected =
        (jtype_injected,
          (polarity_injected, jtype_injected) OCanren.Std.Pair.injected
            GT.option OCanren.ilogic)
          targ_fuly OCanren.ilogic
      and jtype_injected =
        (targ_injected OCanren.Std.List.injected, targ_injected, id_injected,
          int OCanren.ilogic, jtype_injected,
          jtype_injected GT.option OCanren.ilogic,
          jtype_injected OCanren.Std.List.injected) jtype_fuly OCanren.ilogic
      let targ_fmapt f__019_ f__020_ subj__021_ =
        let open OCanren.Env.Monad in
          (((OCanren.Env.Monad.return (GT.gmap targ_fuly)) <*> f__019_) <*>
             f__020_)
            <*> subj__021_
      let jtype_fmapt f__009_ f__010_ f__011_ f__012_ f__013_ f__014_ f__015_
        subj__016_ =
        let open OCanren.Env.Monad in
          ((((((((OCanren.Env.Monad.return (GT.gmap jtype_fuly)) <*> f__009_)
                  <*> f__010_)
                 <*> f__011_)
                <*> f__012_)
               <*> f__013_)
              <*> f__014_)
             <*> f__015_)
            <*> subj__016_
      let rec __jtype fa6 fa5 fa4 fa3 fa2 fa1 fa0 =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun _ ->
               OCanren.prj_exn <..>
                 (chain (jtype_fmapt fa6 fa5 fa4 fa3 fa2 fa1 fa0)))
      and __targ fa1 fa0 =
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun _ -> OCanren.prj_exn <..> (chain (targ_fmapt fa1 fa0)))
      let fix =
        let rec jtype_prj_exn eta =
          (__jtype (OCanren.Std.List.prj_exn targ_prj_exn) targ_prj_exn
             id_prj_exn OCanren.prj_exn jtype_prj_exn
             (OCanren.Std.Option.prj_exn jtype_prj_exn)
             (OCanren.Std.List.prj_exn jtype_prj_exn)) eta
        and targ_prj_exn eta =
          (__targ jtype_prj_exn
             (OCanren.Std.Option.prj_exn
                (OCanren.Std.Pair.prj_exn polarity_prj_exn jtype_prj_exn))) eta in
        (jtype_prj_exn, targ_prj_exn)
      let jtype_prj_exn eta = let (f, _) = fix in f eta
      let targ_prj_exn eta = let (_, f) = fix in f eta
    end
  let rec pp_arg ppf =
    (function
     | Wildcard opt ->
         Format.fprintf ppf "Wildcard %a"
           (([%fmt : (polarity * 'a) GT.option]) pp_typ) opt
     | T l -> Format.fprintf ppf "%a" pp_typ l : targ -> unit)
  and pp_typ ppf =
    (function
     | Array typ ->
         Format.fprintf ppf "(Array %a)"
           (GT.fmt OCanren.Std.List.ground pp_typ) typ
     | Class _ -> Format.fprintf ppf "Class "
     | Interface _ -> Format.fprintf ppf "Interface "
     | Null -> Format.fprintf ppf "Null"
     | V { id } -> Format.fprintf ppf "_.%d" id
     | Intersect _ -> Format.fprintf ppf "Intersect" : jtype -> unit)
  open OCanren
  let () =
    (((let open OCanren in run q) (fun q -> q === (!! (Wildcard (!! None))))
        (fun rr -> rr#reify targ_prj_exn))
       |> OCanren.Stream.take)
      |> (Stdlib.List.iter (Format.printf "%a\n%!" pp_arg))
  let () =
    let open OCanren.Std in
      let exa1 : jtype_injected =
        let a : jtype_injected Std.List.injected =
          (!!
             (V
                { id = (!! 1); index = (!! 2); upb = (!! Null); lwb = (!! None)
                }))
            % (nil ()) in
        !! (Array a) in
      (((run q) (fun q -> q === exa1) (fun rr -> rr#reify jtype_prj_exn)) |>
         OCanren.Stream.take)
        |> (Stdlib.List.iter (Format.printf "%a\n%!" pp_typ))
  $ ./test013mutual.exe
  test012
  Wildcard None
  (Array [ _.1])
