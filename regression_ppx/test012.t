  $ ../ppx/pp_ocanren_all.exe  test012mutual.ml -pretty -new-typenames
  let () = print_endline "test012"
  include
    struct
      type ('a, 'a0) targ_fuly =
        | T of 'a0 * 'a 
        | TNoarg 
      and ('a, 'a1, 'a0) jtyp_fuly =
        | Array of 'a1 
        | V of 'a0 
        | Other of 'a [@@deriving gt ~options:{ gmap }]
      type 'a targ = ('a, 'a jtyp) targ_fuly
      and 'a jtyp = ('a, 'a jtyp, 'a targ) jtyp_fuly
      type 'a targ_logic = ('a, 'a jtyp_logic) targ_fuly OCanren.logic
      and 'a jtyp_logic =
        ('a, 'a jtyp_logic, 'a targ_logic) jtyp_fuly OCanren.logic
      type 'a targ_injected = ('a, 'a jtyp_injected) targ_fuly OCanren.ilogic
      and 'a jtyp_injected =
        ('a, 'a jtyp_injected, 'a targ_injected) jtyp_fuly OCanren.ilogic
      let targ_fmapt f__010_ f__011_ subj__012_ =
        let open OCanren.Env.Monad in
          (((OCanren.Env.Monad.return (GT.gmap targ_fuly)) <*> f__010_) <*>
             f__011_)
            <*> subj__012_
      let jtyp_fmapt f__004_ f__005_ f__006_ subj__007_ =
        let open OCanren.Env.Monad in
          ((((OCanren.Env.Monad.return (GT.gmap jtyp_fuly)) <*> f__004_) <*>
              f__005_)
             <*> f__006_)
            <*> subj__007_
      include
        struct
          let rec __jtyp fa fa1 fa0 =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun _ -> OCanren.prj_exn <..> (chain (jtyp_fmapt fa fa1 fa0)))
          and __targ fa fa0 =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun _ -> OCanren.prj_exn <..> (chain (targ_fmapt fa fa0)))
          let fix =
            let rec jtyp_prj_exn fa eta =
              (__jtyp fa (jtyp_prj_exn fa) (targ_prj_exn fa)) eta
            and targ_prj_exn fa eta = (__targ fa (jtyp_prj_exn fa)) eta in
            (jtyp_prj_exn, targ_prj_exn)
          let jtyp_prj_exn eta = let (f, _) = fix in f eta
          let targ_prj_exn eta = let (_, f) = fix in f eta
        end
      include
        struct
          let rec __jtyp fa fa1 fa0 =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun _ ->
                   OCanren.reify <..>
                     (chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(jtyp_fmapt fa fa1 fa0)))))
          and __targ fa fa0 =
            let open OCanren.Env.Monad in
              OCanren.Reifier.fix
                (fun _ ->
                   OCanren.reify <..>
                     (chain
                        (OCanren.Reifier.zed
                           (OCanren.Reifier.rework ~fv:(targ_fmapt fa fa0)))))
          let fix =
            let rec jtyp_reify fa eta =
              (__jtyp fa (jtyp_reify fa) (targ_reify fa)) eta
            and targ_reify fa eta = (__targ fa (jtyp_reify fa)) eta in
            (jtyp_reify, targ_reify)
          let jtyp_reify eta = let (f, _) = fix in f eta
          let targ_reify eta = let (_, f) = fix in f eta
        end
    end
  let rec pp_arg fa ppf =
    (function
     | TNoarg -> Format.fprintf ppf "noarg"
     | T (l, r) -> Format.fprintf ppf "(%a,%a)" (pp_typ fa) l fa r : 'a targ ->
                                                                      unit)
  and pp_typ fa ppf =
    (function
     | Array typ -> Format.fprintf ppf "(Array %a)" (pp_typ fa) typ
     | V arg -> Format.fprintf ppf "%a" (pp_arg fa) arg
     | Other a -> Format.fprintf ppf "(Other %a)" fa a : 'a jtyp -> unit)
  open OCanren
  let () =
    (((let open OCanren in run q) (fun q -> q === (!! TNoarg))
        (fun rr -> rr#reify (targ_prj_exn OCanren.prj_exn)))
       |> OCanren.Stream.take)
      |>
      (Stdlib.List.iter (Format.printf "%a\n%!" (pp_arg Format.pp_print_int)))
  let () =
    (((let open OCanren in run q)
        (fun q ->
           q === (!! (Array (!! (V (!! (T ((!! (Other (!! 1))), (!! 2)))))))))
        (fun rr -> rr#reify (jtyp_prj_exn OCanren.prj_exn)))
       |> OCanren.Stream.take)
      |>
      (Stdlib.List.iter (Format.printf "%a\n%!" (pp_typ Format.pp_print_int)))
  $ ./test012mutual.exe
  test012
  noarg
  (Array ((Other 1),2))
