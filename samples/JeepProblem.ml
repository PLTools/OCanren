(*
 * "Jeep Problem" (https://en.wikipedia.org/wiki/Jeep_problem)
 * Copyright (C) 2016-2020
 * Dmitri Boulytchev, Peter Lozov
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open GT

module L = List

open OCanren
open OCanren.Std

(* Types of moves *)
@type 'nat move =
  Forward  of 'nat
| Backward of 'nat
| Unload   of 'nat
| Fill     of 'nat
with show, gmap;;

(* List of moves *)
@type moves = int move GT.list with show;;
(* ... logically *)
@type lmoves = ocanren {int move GT.list} with show;;
                                                                                                                                         
(* State: distance, amount of fuel, list of fuel dumps *)
@type state = int * int * (int * int) GT.list with show;;
(* ... logically *)                                                        
@type lstate = ocanren {int * int * (int * int) GT.list} with show
                                                                                                    
(* Reification primitives *)
module M = Fmap (struct type 'a t = 'a move let fmap f = gmap(move) f end)

let forward  x = inj @@ M.distrib (Forward  x)
let backward x = inj @@ M.distrib (Backward x)
let unload   x = inj @@ M.distrib (Unload   x)
let fill     x = inj @@ M.distrib (Fill     x)                                  

(* Lookups a station:
     d       : a distance
     stations: a list of stations
     q       : q --- an optional amount of fuel at the station
*)
let rec lookupo d stations q =
  let open Nat in
  ocanren {
    stations == [] & q == None |
    fresh d', q', ss in
      stations == (d', q') :: ss &
      {d' == d & Some q' == q |
       d' <  d & lookupo d ss q |
       d' >  d & q == None        
      }
  }

(* Tests if a number is positive *)
let positive n = ocanren {fresh x in n == Nat.succ x}

(* Puts a station into stations' list:
     stations : a list of existing stations
     d        : a distance to make the station at
     q        : amount of fuel
     stations': a list of new stations
*)
let rec puto stations d q stations' =
  let open Nat in
  ocanren {  
    stations == [] & {q == 0 & stations' == [] | positive q & stations' == [(d, q)]} |
    fresh d', q', ss, ss' in
      stations == (d', q') :: ss &
      {{d' == d & {q == 0 & stations' == ss | positive q & stations' == (d, q) :: ss}} |
       {d' <  d & puto ss d q ss' & stations' == (d', q') :: ss'} |
       {d' >  d & {q == 0 & stations' == stations | positive q & stations' == (d, q) :: stations}}
      }
    }

let max_capacity = nat 5

(* Performs a single step *)                 
let step state m state' =
  let open Nat in
  ocanren {
    fresh pos, gas, stations in
      state == (pos, gas, stations) &
      {
        fresh d, pos', gas' in
          m      == Forward d &
          state' == (pos', gas', stations) &
          d      <= gas  &
          (+) pos d pos' &
          (+) gas' d gas
        
      | fresh d, pos', gas' in
          m      == Backward d &
          state' == (pos', gas', stations) &
          d      <= gas &
          (+) pos' d pos &
          (+) gas' d gas
        
      | fresh q, gas', stations' in
          m      == Unload q &
          q      <= max_capacity &
          state' == (pos, gas', stations') &
          q      <= gas &
          (+) q gas' gas &
          {lookupo pos stations None & puto stations pos q stations' |
           fresh q', q'' in
             lookupo pos stations (Some q') & (+) q' q q'' & puto stations pos q'' stations'
          }
       
       | fresh q, gas', q', q'', stations' in
          m == Fill (q) & q <= max_capacity &
          {pos    == 0 &
           state' == (pos, gas', stations) &
           (+) gas q gas' &
           gas' <= max_capacity |
             
           positive pos &
           state' == (pos, gas', stations') &
           lookupo pos stations (Some q') & 
           q' <= q &
           (+) gas q gas' &
           gas' <= max_capacity &
           (+) q'' q q' &
           puto stations pos q'' stations'}
      }
    }
            
let kind m k =
  ocanren {
    fresh n in
      {m == Forward n | m == Backward n} & n =/= 0 & k == !(!!0)
    | {m == Fill    n | m == Unload   n} & n =/= 0 & k == !(!!1)
  }

(* Performs multiple steps *)
let steps state moves state' =
  let steps = Tabling.(tabledrec four) (fun steps k state moves state' ->
    ocanren {
      moves == [] & state == state' |
      fresh state'', m, moves', k' in
        moves == m :: moves' &
        kind m k' &
        k' =/= k &
        step state m state'' &
        steps k' state'' moves' state'      
    })
  in
  steps !!2 state moves state'

let prj_moves x = List.to_list (gmap(move) Nat.to_int) (project x)
let prj_state x =
  let x, (y, z) = project x in
  (Nat.to_int x, Nat.to_int y, List.to_list (fun (x, y) -> Nat.to_int x, Nat.to_int y) z)
                
let init = pair (nat 0) @@ pair max_capacity (nil ())

let _ =
  L.iter (fun q -> Printf.printf "Reaching 6: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {steps init [Forward 2; Unload 1; Backward 2; Fill 5; Forward 2; Fill 1; Forward 4] q}) prj_state;

  L.iter (fun q -> Printf.printf "Making stations: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {steps init [Forward 1; Unload 2; Backward 1; Fill 3; Forward 2] q}) prj_state;

  L.iter (fun q -> Printf.printf "Searching for making stations: %s\n%!" @@ show(moves) q) @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {steps init q (2, 2, [(1, 2)])}) prj_moves;

  L.iter (fun q -> Printf.printf "Searching for reaching 6: %s\n%!" @@ show(moves) q) @@ Stream.take ~n:1 @@
    run q (fun q -> ocanren {steps init q (6, 0, [])}) prj_moves;

  L.iter (fun q -> Printf.printf "Reaching 8: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {steps init [Forward 2; Unload 1; Backward 2; Fill 3; Forward 1; Unload 1; Backward 1; 
                                       Fill 5; Forward 2; Unload 1; Backward 2; Fill 5; Forward 1; 
                                       Fill 1; Forward 1; Fill 1; Forward 1; Unload 2; Backward 1; Fill 1; Backward 2; 
                                       Fill 3; Forward 1; Unload 1; Backward 1; Fill 5; Forward 1; Fill 1; 
                                       Forward 2; Fill 2; Forward 5] q}) prj_state;

  L.iter (fun q -> Printf.printf "Searching for reaching 8: %s\n%!" @@ show(moves) q) @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {fresh r, s in steps init q (8, r, s)}) prj_moves;

