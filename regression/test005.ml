open Printf
open GT

module L = List

open OCanren
open OCanren.Std
open Tester
open Stlc

module GTyp = struct
  @type ('a, 'b) t =
  | P   of 'a      (* primitive *)
  | Arr of 'b * 'b (* arrow *)
  with gmap, show

  let fmap f g x = gmap(t) f g x

  type rtyp = (string, rtyp) t
  type ltyp = (string logic, ltyp) t logic
  type injected = (string ilogic, injected) t ilogic

  let p s     : injected = inj @@ P s
  let arr x y : injected = inj @@ Arr (x,y)

  let rec show_rtyp typ = show(t) (show string) show_rtyp typ
  let rec show_ltyp typ = show(logic) (show(t) (show(logic) @@ show string) show_ltyp) typ

  let reify : (injected, ltyp) Reifier.t =
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun fself ->
      Reifier.compose Reifier.reify
        (Reifier.reify >>= fun rstring ->
        fself >>= fun fr ->
        let rec foo = function
          | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
          | Value x -> Value (GT.gmap t rstring fr x)
        in
        Env.Monad.return foo
    ))

  let prj_exn : (injected, rtyp) Reifier.t =
    let ( >>= ) = Env.Monad.bind in
    Reifier.fix (fun self ->
      Reifier.compose Reifier.prj_exn
      ( self >>= fun fr ->
        Reifier.prj_exn >>= fun rstring ->
        Env.Monad.return (fun x -> GT.gmap t rstring fr x))
      )
end

open GLam
open GTyp

let rec lookupo a g t =
  Fresh.three (fun a' t' tl ->
    (g === (pair a' t')%tl) &&&
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])
  )

let infero expr typ =
  let rec infero gamma expr typ =
    conde [
      Fresh.one (fun x ->
        (expr === v x) &&&
        (lookupo x gamma typ));
      Fresh.three (fun m n t ->
        (expr === app m n) &&&
        (infero gamma m (arr t typ)) &&&
        (infero gamma n t));
      Fresh.four (fun x l t t' ->
        (expr === abs x l) &&&
        (typ  === arr t t') &&&
        (infero ((pair x t)%gamma) l t'))
    ]
  in
  infero (nil()) expr typ

let show_string  = show(string)
let show_stringl = show(logic) (show(string))

let inj_list_p xs = Std.list (fun (x,y) -> pair x y) xs

(* Without free variables *)
let run_lam eta = run_r GLam.prj_exn GLam.show_rlam eta
let run_string eta = run_r OCanren.prj_exn (GT.show GT.string) eta
let run_typ eta = run_r GTyp.prj_exn GTyp.show_rtyp eta

let () =
  run_lam    1 q qh (REPR (fun q -> lookupo varX (inj_list_p [])  q                                   ));
  run_lam    1 q qh (REPR (fun q -> lookupo varX (inj_list_p [(varX, v varX)]) q                    ));
  run_lam    1 q qh (REPR (fun q -> lookupo varX (inj_list_p [(varY, v varY); (varX, v varX)]) q    ));

  run_string  1 q qh (REPR (fun q -> lookupo    q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varX)  ));
  run_string  1 q qh (REPR (fun q -> lookupo    q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varY)  ));

  run_typ    1 q qh (REPR (fun q -> infero (abs varX (app (v varX) (v varX)))                q))

let show_env_logic = show(List.logic) @@ show(Pair.logic) (show(logic) (fun s -> s)) show_llam

let env_reifier = List.reify (Pair.reify OCanren.reify GLam.reify)

let show_env  = show(List.ground) @@ show(Pair.ground) show_string  GLam.show_rlam
let show_envl = show(List.logic ) @@ show(Pair.logic) show_stringl GLam.show_llam

let runEnv n = run_r env_reifier show_envl n

let runT n = run_r GTyp.reify GTyp.show_ltyp n
let runL n = run_r GLam.reify GLam.show_llam n

let () =
  runEnv   1   q   qh (REPR (fun q -> lookupo varX q (v varY)                                       ));
  runT     1   q   qh (REPR (fun q -> infero (abs varX (v varX)) q                                  ));
  runT     1   q   qh (REPR (fun q -> infero (abs varF (abs varX (app (v varF) (v varX)))) q        ));
  runT     1   q   qh (REPR (fun q -> infero (abs varX (abs varF (app (v varF) (v varX)))) q        ));
  runL     1   q   qh (REPR (fun q -> infero q (arr (p varX) (p varX))                              ))
