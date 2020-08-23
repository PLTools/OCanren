open GT

module L = List

open OCanren
open OCanren.Std

@type 'nat move = Forward of 'nat | Backward of 'nat | Unload of 'nat | Fill of 'nat with show, gmap;;

let () = ();;

@type moves = ocanren (int move list) with show;;
@type state = ocanren (int * int * (int * int) list) with show

module M = Fmap (struct type 'a t = 'a move let fmap f = gmap(move) f end)

let reify_moves m = List.reify (M.reify Nat.reify) m
let reify_stations s = List.reify (Pair.reify Nat.reify Nat.reify) s
let reify_state s = Pair.reify Nat.reify (Pair.reify Nat.reify reify_stations) s

let forward  x = inj @@ M.distrib (Forward  x)
let backward x = inj @@ M.distrib (Backward x)
let unload   x = inj @@ M.distrib (Unload   x)
let fill     x = inj @@ M.distrib (Fill     x)

let rec lookupo d stations q =
  conde [
    stations === nil () &&& (q === none ());
    fresh (d' q' ss) (
      stations === (pair d' q') % ss &&&
      ((d' === d &&& (some q' === q)) |||
       (Nat.(<) d' d &&& lookupo d ss q) |||
       (Nat.(>) d' d &&& (q === none ()))
      )
    )
  ]

let notZero n = fresh (x) (n === Nat.succ x)

let rec puto stations d q stations' =
  conde [
    stations === nil () &&& conde [q === Nat.zero &&& (stations' === nil ()); notZero q &&& (stations' === !< (pair d q))];
    fresh (d' q' ss ss') (
      stations === (pair d' q') % ss &&&
      ((d' === d &&& conde [q === Nat.zero &&& (stations' === ss); notZero q &&& (stations' === (pair d q) % ss)]) |||
       (Nat.(<) d' d &&& puto ss d q ss' &&& (stations' === (pair d' q') % ss')) |||
       (Nat.(>) d' d &&& conde [q === Nat.zero &&& (stations' === stations); notZero q &&& (stations' === (pair d q) % stations)])
      )
    )
  ]

let triple a b c = pair a @@ pair b c

let max_capacity = nat 5

let step state m state' =
  fresh (pos gas stations) (
    state === triple pos gas stations &&&
    conde [
      fresh (d pos' gas') (m === forward  d &&& (state' === triple pos' gas' stations) &&& Nat.(<=) d gas &&& Nat.addo pos d pos' &&& Nat.addo gas' d gas);
      fresh (d pos' gas') (m === backward d &&& (state' === triple pos' gas' stations) &&& Nat.(<=) d gas &&& Nat.addo pos' d pos &&& Nat.addo gas' d gas);
      fresh (q gas' stations') (
        m === unload q &&& Nat.(<=) q max_capacity &&& (state' === triple pos gas' stations') &&& Nat.(<=) q gas &&& Nat.addo q gas' gas &&&
        ((lookupo pos stations (none ()) &&& puto stations pos q stations') |||
         fresh (q' q'') (
           lookupo pos stations (some q') &&& (Nat.addo q' q q'' &&& puto stations pos q'' stations')
         )
        )
      );
      fresh (q gas' q' q'' stations') (
        m === fill q &&& Nat.(<=) q max_capacity &&&
        conde [
          pos === Nat.zero &&&
          (state' === triple pos gas' stations) &&&
          Nat.addo gas q gas' &&&
          Nat.(<=) gas' max_capacity;
          notZero pos &&&
          (state' === triple pos gas' stations') &&&
          lookupo pos stations (some q') &&&
          Nat.(>=) q' q &&&
          Nat.addo gas q gas' &&&
          Nat.(<=) gas' max_capacity &&&
          Nat.addo q'' q q' &&&
          puto stations pos q'' stations'
        ]
      )
    ]
  )

let rec inj_moves = function
| []      -> nil ()
| h :: tl -> (match h with Forward n -> forward @@ nat n | Backward n -> backward @@ nat n | Fill n -> fill @@ nat n | Unload n -> unload @@ nat n) % inj_moves tl

let kind m k =
  fresh (n) (
    conde [
      (m === forward n ||| (m === backward n)) &&& (n =/= Nat.zero) &&& (k === !!0);
      (m === fill    n ||| (m === unload   n)) &&& (n =/= Nat.zero) &&& (k === !!1);
    ]
  )

let steps state moves state' =
  let steps = Tabling.(tabledrec four) (fun steps k state moves state' ->
    conde [
      moves === nil () &&& (state === state');
      fresh (state'' m moves' k') (
        moves === m % moves' &&&
        kind m k' &&&
        (k' =/= k) &&&
        (step state m state'' &&&
        steps k' state'' moves' state')
      )
    ])
  in
  steps !!2 state moves state'

let init = triple (nat 0) max_capacity (nil ())
let id x = x

let _ =
  L.iter (fun q -> Printf.printf "Reaching 6: %s\n%!" (show(state) (q#reify reify_state))) @@ Stream.take ~n:1 @@
  run q (fun q -> steps init (inj_moves [Forward 2; Unload 1; Backward 2; Fill 5; Forward 2; Fill 1; Forward 4]) q) id;

  L.iter (fun q -> Printf.printf "Making stations: %s\n%!" (show(state) (q#reify reify_state))) @@ Stream.take ~n:1 @@
  run q (fun q -> steps init (inj_moves [Forward 1; Unload 2; Backward 1; Fill 3; Forward 2]) q) id;

  L.iter (fun q -> Printf.printf "Searching for making stations: %s\n%!" (show(moves) (q#reify reify_moves))) @@ Stream.take ~n:1 @@
  run q (fun q -> steps init q (triple (nat 2) (nat 2) (!< (pair (nat 1) (nat 2))))) id;

  L.iter (fun q -> Printf.printf "Searching for reaching 6: %s\n%!" (show(moves) (q#reify reify_moves))) @@ Stream.take ~n:1 @@
  run q (fun q -> steps init q (triple (nat 6) (nat 0) (nil ()))) id;

  L.iter (fun q -> Printf.printf "Reaching 8: %s\n%!" (show(state) (q#reify reify_state))) @@ Stream.take ~n:1 @@
  run q (fun q -> steps init (inj_moves [Forward 2; Unload 1; Backward 2; Fill 3; Forward 1; Unload 1; Backward 1;
                                         Fill 5; Forward 2; Unload 1; Backward 2; Fill 5; Forward 1;
                                         Fill 1; Forward 1; Fill 1; Forward 1; Unload 2; Backward 1; Fill 1; Backward 2;
                                         Fill 3; Forward 1; Unload 1; Backward 1; Fill 5; Forward 1; Fill 1;
                                         Forward 2; Fill 2; Forward 5]) q) id;

  L.iter (fun q -> Printf.printf "Searching for reaching 8: %s\n%!" (show(moves) (q#reify reify_moves))) @@ Stream.take ~n:1 @@
  run q (fun q -> fresh (r s) (steps init q (triple (nat 8) r s))) id;
