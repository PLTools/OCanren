open OCanren
open Tester

let () = Printexc.record_backtrace false

module _ = struct
  include struct
    type nonrec 'a t =
      | Z
      | S of 'a
    [@@deriving gt ~options:{ gmap; show }]

    type ground = ground t [@@deriving gt ~options:{ gmap; show }]
    type logic = logic t OCanren.logic [@@deriving gt ~options:{ gmap; show }]
    type injected = injected t OCanren.ilogic

    let prj_exn =
      let open Env.Monad.Syntax in
      Reifier.fix (fun rself ->
        Reifier.compose
          OCanren.prj_exn
          (let* self = rself in
           Env.Monad.return (GT.gmap t self)))
    ;;

    (* good reifier *)
    let reify : (injected, logic) Reifier.t =
      let open Env.Monad.Syntax in
      Reifier.fix (fun rself ->
        Reifier.compose
          OCanren.reify
          (let* self = rself in
           let rec foo = function
             | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
             | Value x -> Value ((GT.gmap t self) x)
           in
           Env.Monad.return foo))
    ;;

    (* trying to make bad  reifier *)
    let reify_bad : (injected, injected t OCanren.logic) Reifier.t =
      let open Env.Monad.Syntax in
      Reifier.fix (fun rself ->
        Reifier.compose
          OCanren.reify
          (let* _self = rself in
           let rec foo = function
             | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
             | Value x -> Value ((GT.gmap t Fun.id) x)
           in
           Env.Monad.return foo))
    ;;

    let z () = OCanren.inji Z
    let succ _x__001_ = OCanren.inji (S _x__001_)
  end

  let run_peano n = run_r reify (GT.show logic) n
  let () = run_peano 1 q qh (REPR (fun q -> q === z ()))

  let v : injected =
    match
      run q (fun q -> fresh m (q === succ m)) (fun rr -> rr#reify reify_bad) |> Stream.hd
    with
    | Value (S var) -> var
    | _ -> assert false
  ;;

  let () =
    try run_peano 1 q qh (REPR (fun q -> q === v)) with
    | Failure s -> Format.printf "Failure: %s\n%!" s
  ;;

  let v : injected t OCanren__Logic.logic =
    run q (fun q -> fresh () (q === succ v)) (fun rr -> rr#reify reify_bad) |> Stream.hd
  ;;
end
