(* SPDX-License-Identifier: LGPL-2.1-or-later *)
open GT

module L = List

open OCanren
open OCanren.Std

ocanren type nonrec move = Empty | Goat | Wolf | Cabbage

let (!) = inj

let qua x y z t = OCanren.inj (x, y, z, t)

[@@@ocaml.warning "-8"]

let [[isGoat; isWolf; isCabbage; isMan] as are; [noGoat; noWolf; noCabbage; noMan] as no] =
  L.map (fun f -> L.map (fun p s -> p f s) [(fun f s -> fresh (x y z) (s === qua !f x y z));
                                            (fun f s -> fresh (x y z) (s === qua x !f y z));
                                            (fun f s -> fresh (x y z) (s === qua x y !f z));
                                            (fun f s -> fresh (x y z) (s === qua x y z !f))]
           ) [true; false]

[@@@ocaml.warning "+8"]

let safe state = fresh (left right) (
                   (state === pair left right) &&&
                   (let safe' side =
                      isMan side |||
                      (noMan side &&&
                         (noGoat side ||| (isGoat side &&& ((noCabbage side &&& noWolf side) |||
                                                            (isCabbage side &&& isWolf side))
                                          )
                         )
                      )
                    in
                    safe' left &&& safe' right
                   )
                 )

let swap state state' =
  fresh (left right) (
    (state === pair left right) &&& (state' === pair right left)
  )

let step state move state' =
  fresh (left right) (
    (state === pair left right) &&&
    let step' left right state' =
      fresh (lm lg lw lc rm rg rw rc) (
        (left === qua lg lw lc lm) &&& (right === qua rg rw rc rm) &&&
        conde [
          (move === !Empty  )                    &&& (state' === pair (qua lg lw lc !false    ) (qua rg rw rc !true))    &&& safe state';
          (move === !Goat   ) &&& isGoat    left &&& (state' === pair (qua !false lw lc !false) (qua !true rw rc !true)) &&& safe state';
          (move === !Wolf   ) &&& isWolf    left &&& (state' === pair (qua lg !false lc !false) (qua rg !true rc !true)) &&& safe state';
          (move === !Cabbage) &&& isCabbage left &&& (state' === pair (qua lg lw !false !false) (qua rg rw !true !true)) &&& safe state';
        ]
      )
    in
    conde [
      isMan left  &&& noMan right &&& step' left right state';
      isMan right &&& noMan left  &&& (fresh (state'') (step' right left state'' &&& swap state'' state'))
    ]
  )

let rec eval state moves state' =
  conde [
    (moves === nil ()) &&& (state === state');
    fresh (move moves' state'') (
      (moves === move % moves') &&&
      (step state move state'') &&&
      (eval state'' moves' state')
    )
    ]


(* module T1 = struct
  [%% distrib
    type nonrec t = A [@@deriving gt ~options:{gmap}]
    type ground = t]
end

module T2 = struct
  [%%distrib
  type nonrec 'a t = B of 'a [@@deriving gt ~options:{gmap}]
  type ground =  T1.ground t ]
end *)

ocanren type state    = (GT.bool * GT.bool * GT.bool * GT.bool) * (GT.bool * GT.bool * GT.bool * GT.bool);;
(* type solution = move logic Std.List.logic [@@deriving gt ~options:{show}] *)
ocanren type solution = move Std.List.ground

let _ =
  let wrap1 rel =
    OCanren.(run q) rel (fun s -> s#reify state_prj_exn)
    |> Stream.iter (fun s -> print_endline @@ show(state) s)
  in
  wrap1 (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (nil ()) q);

  wrap1 (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (!< !Empty) q);

  wrap1 (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (!< !Goat) q);

  wrap1 (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (!< !Wolf) q)

let () =
  L.iter (fun s -> Printf.printf "%s\n" @@ show(solution) s) @@ Stream.take ~n:100 @@
  run q (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) q (pair (qua !false !false !false !false) (qua !true !true !true !true))) (fun s -> s#reify solution_prj_exn)
