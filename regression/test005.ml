open Printf
open GT

module L = List

open OCanren
open OCanren.Std
open Tester
open Stlc

module GTyp =
  struct

    module T =
      struct
        @type ('a, 'b) t =
        | P   of 'a      (* primitive *)
        | Arr of 'b * 'b (* arrow *)
        with gmap, show

       let fmap f g x = gmap(t) f g x
     end

  include T
  include Fmap2 (T)

  type rtyp = (string, rtyp) t
  type ltyp = (string logic, ltyp) t logic
  type ftyp = (rtyp, ltyp) injected

  let p s     : ftyp = inj @@ distrib @@ P s
  let arr x y : ftyp = inj @@ distrib @@ Arr (x,y)

  let rec show_rtyp typ = show(t) (show string) show_rtyp typ
  let rec show_ltyp typ = show(logic) (show(t) (show(logic) @@ show string) show_ltyp) typ

end

let rec gtyp_reifier c x = GTyp.reify OCanren.reify gtyp_reifier c x

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

let inj_list_p xs = List.list @@ L.map (fun (x,y) -> pair x y) xs

(* Without free variables *)
let () =
  run_exn GLam.show_rlam    1 q qh (REPR (fun q -> lookupo varX (inj_list_p [])  q                                   ));
  run_exn GLam.show_rlam    1 q qh (REPR (fun q -> lookupo varX (inj_list_p [(varX, v varX)]) q                    ));
  run_exn GLam.show_rlam    1 q qh (REPR (fun q -> lookupo varX (inj_list_p [(varY, v varY); (varX, v varX)]) q    ));

  run_exn (show string)  1 q qh (REPR (fun q -> lookupo    q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varX)  ));
  run_exn (show string)  1 q qh (REPR (fun q -> lookupo    q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varY)  ));

  run_exn GTyp.show_rtyp    1 q qh (REPR (fun q -> infero (abs varX (app (v varX) (v varX)))                q))

let show_env_logic = show(List.logic) @@ show(Pair.logic) (show(logic) (fun s -> s)) show_llam

let pair c p = Pair.reify OCanren.reify glam_reifier c p

let env_reifier c xs = List.reify pair c xs

let show_env  = show(List.ground) @@ show(Pair.ground) show_string  GLam.show_rlam
let show_envl = show(List.logic ) @@ show(Pair.logic) show_stringl GLam.show_llam

let runEnv n = runR env_reifier show_env show_envl n

let runT n = runR gtyp_reifier GTyp.show_rtyp GTyp.show_ltyp n
let runL n = runR glam_reifier GLam.show_rlam GLam.show_llam n

let () =
  runEnv   1   q   qh (REPR (fun q -> lookupo varX q (v varY)                                       ));
  runT     1   q   qh (REPR (fun q -> infero (abs varX (v varX)) q                                  ));
  runT     1   q   qh (REPR (fun q -> infero (abs varF (abs varX (app (v varF) (v varX)))) q        ));
  runT     1   q   qh (REPR (fun q -> infero (abs varX (abs varF (app (v varF) (v varX)))) q        ));
  runL     1   q   qh (REPR (fun q -> infero q (arr (p varX) (p varX))                              ))
