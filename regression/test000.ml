(* Testing Option.t and Result.t here *)
open OCanren
open OCanren.Std
open Tester
open Printf
open GT

let show_int       = show(int)
let show_int_opt   = show(option) (show(int))
let show_intl      = show(logic)  (show(int))
let show_intl_optl = show(logic)  (show(option) (show(logic) (show(int))))

let run_opt eta = run_r (Option.reify OCanren.reify) show_intl_optl eta
let run_int eta = run_r OCanren.prj_exn show_int eta

let _ = Option.(
    run_int 1 q qh (REPR(fun q -> q === !!5));
    run_opt 1 q qh (REPR(fun q -> q === some !!5));
    run_opt 1 q qh (REPR(fun q -> q === none ()));
    run_int 1 q qh (REPR(fun q -> some q === some !!5 ));
    run_opt 1 q qh (REPR(fun q -> fresh (w) (q === some w) ))
  )

module Result = struct
  @type ('a, 'b) t = ('a, 'b) Result.t =
    | Ok of 'a | Error of 'b
    with gmap,show

  @type ('a, 'b) logic = ('a, 'b)  t OCanren.logic
    with gmap,show

  type ('a, 'b) groundi = ('a, 'b) t ilogic

  let reify : ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t -> (('a,'c) groundi, ('b,'d) logic) Reifier.t =
    fun ra rb ->
    let ( >>= ) = Env.Monad.bind in
    Reifier.compose Reifier.reify
    (Reifier.reify >>= fun r ->
    ra >>= fun fa ->
    rb >>= fun fb ->
    let rec foo x =
      match x with
      | Var (v, xs) ->
        Var (v, Stdlib.List.map foo xs)
      | Value x -> Value (GT.gmap t fa fb x)
      in
    Env.Monad.return foo)

  let prj_exn : 'a 'b. ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t -> (('a,'c) groundi, ('b,'d) t) Reifier.t
      =
    fun ra rb ->
      let ( >>= ) = Env.Monad.bind in
      Reifier.prj_exn >>= fun r ->
      ra >>= fun fa ->
      rb >>= fun fb ->
        Env.Monad.return (fun x -> GT.gmap t fa fb (r x))

  let ok x    = inj (Ok x)
  let error x = inj (Error x)
end

let show1 = show(Result.t) (show(int)) (show(option) (show(int)))
let show1logic =
  show(logic) (show(Result.t)
    (show(logic) (show int))
    (show(logic) (show int)) )

let runResult n =
  run_r (Result.reify OCanren.reify OCanren.reify) show1logic n

let _ =
  runResult     1  q qh (REPR(fun q -> q === Result.ok !!5 ));
  runResult   (-1) q qh (REPR(fun q ->
    fresh (r)
      (q === Result.ok r)
      (conde [r === !!5; success])
    ));
  runResult   (-1) q qh (REPR(fun q -> fresh (r s)
    (conde
      [ (q === Result.ok    s) &&& (s =/= !!4)
      ; (q === Result.error r)
      ])
  ))
