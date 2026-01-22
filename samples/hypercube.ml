(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * Tree: finite set implementation using HyperCubes.
 * Copyright (C) 2022-2026
 * Dmitri Boulytchev, Dmitrii Kosarev, Petr Lozov
 * St.Petersburg State University
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

open OCanren

module Rel_kind = struct
  [@@@ocaml.warning "-27"]

  [%%ocanren_inject
  type nonrec t = R1 | R2 | R3 | R4
  [@@deriving gt ~options:{ fmt; show; gmap }]

  type ground = t]

  let inj : ground -> injected = ( !! )
  let to_ground = OCanren.from_logic
end

module Rel = struct
  [@@@ocaml.warning "-27"]

  [%%ocanren_inject
  type nonrec ('kind, 'a) t = Bin_rel of 'kind * 'a * 'a
  [@@deriving gt ~options:{ show; gmap }]

  type 'a ground = (Rel_kind.ground, 'a) t]

  let inj inj_kind inj_id = function
    | Bin_rel (kind, l, r) -> bin_rel (inj_kind kind) (inj_id l) (inj_id r)

  let r1 l r = bin_rel !!Rel_kind.R1 l r
  let r2 l r = bin_rel !!Rel_kind.R2 l r
  let r3 l r = bin_rel !!Rel_kind.R3 l r
  let r4 l r = bin_rel !!Rel_kind.R4 l r

  let ground =
    {
      ground with
      plugins =
        object
          method show fe : _ ground -> string =
            function
            | Bin_rel (k, l, r) ->
                Format.asprintf "%s (%s, %s)"
                  (GT.show Rel_kind.ground k)
                  (fe l) (fe r)
        end;
    }

  let logic =
    {
      logic with
      plugins =
        object
          method show fe : _ logic -> string =
            fun eta ->
              GT.show OCanren.logic
                (function
                  | Bin_rel (k, l, r) ->
                      Format.asprintf "%s (%s, %s)" (GT.show Rel_kind.logic k)
                        (fe l) (fe r))
                eta
        end;
    }

  let split = function Bin_rel (k, a, b) -> (k, a, b)
  let make_rel inj_id k a b = bin_rel k (inj_id a) (inj_id b)

  let make k a b =
    let _ : Rel_kind.t = k in
    Bin_rel (k, a, b)
end

module Compose = struct
  type nonrec 'a t = { r1 : 'a; r2 : 'a; r3 : 'a; r4 : 'a }
  [@@deriving gt ~options:{ fmt; show; gmap }]

  type 'a logic = 'a t OCanren.logic [@@deriving gt ~options:{ show }]

  open Rel_kind

  let get_by_comp cc = function
    | R1 -> cc.r1
    | R2 -> cc.r2
    | R3 -> cc.r3
    | R4 -> cc.r4

  let set_by_comp ss id v =
    match id with
    | R1 -> { ss with r1 = v }
    | R2 -> { ss with r2 = v }
    | R3 -> { ss with r3 = v }
    | R4 -> { ss with r4 = v }

  let mapi_cc f cs =
    { r1 = f R1 cs.r1; r2 = f R2 cs.r2; r3 = f R3 cs.r3; r4 = f R4 cs.r4 }
end

type 'a comp_list = 'a Rel.injected Std.List.injected
type 'id lkey = (Rel_kind.injected, 'id ilogic) Rel.t ilogic

module type S = sig
  type 'a t
  type id = string
  type iid = id ilogic

  (* Non-relational interface  *)

  val init_hypercube : id list -> (id Rel.ground -> 'a) -> 'a t
  val get_element : 'v t -> id Rel.ground -> 'v
  val set_element : 'v t -> id Rel.ground -> 'v -> 'v t
  val mapi : (id Rel.ground -> 'a -> 'b) -> 'a t -> 'b t

  (* Currently not used  *)
  (* val map : ('a -> 'b) -> 'a t -> 'b t *)

  val foldi : ('acc -> 'v -> id Rel.ground -> 'acc) -> 'acc -> 'v t -> 'acc

  (** Relational interface *)

  val fresh_hypercube : id list -> ('v ilogic t -> goal) -> goal
  val get_element_rel : 'v ilogic t -> id lkey -> 'v ilogic -> goal

  val set_element_rel :
    'v ilogic t -> id lkey -> 'v ilogic -> ('v ilogic t -> goal) -> goal

  val map_rel :
    ('value1 ilogic -> 'value2 ilogic -> goal) ->
    'value1 ilogic t ->
    'value2 ilogic t ->
    goal

  (* Despite other, this relation doesn't return goal to support high-order relations *)
  val foldi_rel :
    ('acc -> 'v ilogic -> id lkey -> 'acc) -> 'acc -> 'v ilogic t -> 'acc

  val to_list :
    'v ilogic t ->
    ('v ilogic -> id lkey -> iid comp_list -> goal) ->
    iid comp_list ->
    goal

  val hypercube_with_properties_to_list :
    iid comp_list t -> iid comp_list -> goal

  (* val hypercube_with_properties_to_list :
     ( int ilogic,
       Composition_property.injected Std.List.injected )
     Std.Pair.groundi
     Std.List.injected
     t ->
     iid comp_list ->
     goal *)
end

type 'value array_t = {
  map : (string * int) list;
  cube : 'value Array.t Compose.t Array.t;
      (* TODO: Why compositions in the middle? *)
}

module Array_hypercubeImpl = struct
  type id = string
  type iid = id ilogic
  type 'a t = 'a array_t

  let inj_id id = OCanren.( !! ) id

  let init_hypercube ids init : _ t =
    let map_array = Array.of_list ids in

    let level1 id1 comp =
      Array.map (fun id2 -> init (Rel.make comp id1 id2)) map_array
    in
    {
      map = List.mapi (fun i x -> (x, i)) ids;
      cube =
        Array.map
          (fun id1 ->
            Compose.
              {
                r1 = level1 id1 Rel_kind.R1;
                r2 = level1 id1 R2;
                r3 = level1 id1 R3;
                r4 = level1 id1 R4;
              })
          map_array;
    }

  let fresh_hypercube ids (k : _ t -> goal) : goal =
    let size = List.length ids in
    let map = List.mapi (fun i x -> (x, i)) ids in
    let ( let* ) x f = x f in
    let fresh_array fresher k =
      let array = Array.init size (Fun.const None) in
      let rec helper i =
        if i < size then (
          let* q = fresher in
          Array.set array i (Some q);
          helper (i + 1))
        else k (Array.map Option.get array)
      in
      helper 0
    in
    let comps k =
      let* r1 = fresh_array call_fresh in
      let* r2 = fresh_array call_fresh in
      let* r3 = fresh_array call_fresh in
      let* r4 = fresh_array call_fresh in
      k Compose.{ r1; r2; r3; r4 }
    in
    fresh_array comps (fun c -> k { map; cube = c })

  let get_element hypercube comp =
    let name, id1, id2 = Rel.split comp in
    let id1_index = List.assoc id1 hypercube.map in
    let id2_index = List.assoc id2 hypercube.map in
    Array.get
      (Compose.get_by_comp (Array.get hypercube.cube id1_index) name)
      id2_index

  let set_element hypercube comp v =
    let name, id1, id2 = Rel.split comp in
    let id1_index = List.assoc id1 hypercube.map in
    let id2_index = List.assoc id2 hypercube.map in
    let cc = Array.get hypercube.cube id1_index in
    let arr = Compose.get_by_comp cc name in
    Array.set arr id2_index v;
    let cc = Compose.set_by_comp cc name arr in
    Array.set hypercube.cube id1_index cc;
    hypercube

  let get_element_rel hypercube comp res =
    let _ : (_, id ilogic) Rel.t ilogic = comp in
    let ids : _ list = List.map fst hypercube.map in

    let get_ids = function Value id_val -> [ id_val ] | Var _ -> ids in
    debug_var comp (Rel.reify OCanren.reify) (fun tested_comp ->
        let comps =
          match tested_comp with
          | [ Value (Rel.Bin_rel (Value name, id1, id2)) ] ->
              let ids1 = get_ids id1 in
              let ids2 = get_ids id2 in
              List.concat_map
                (fun id1 -> List.map (Rel.make name id1) ids2)
                ids1
          | [ Var _ ] ->
              List.concat_map
                (fun name ->
                  List.concat_map
                    (fun id1 -> List.map (Rel.make name id1) ids)
                    ids)
                [ R1; R2; R3; R4 ]
          | _ -> failwith "Unexpected behaviour for 'debug_var'."
        in
        conde
        @@ List.map
             (fun ground_comp ->
               fresh ()
                 (comp === Rel.inj Rel_kind.inj OCanren.inj ground_comp)
                 (res === get_element hypercube ground_comp))
             comps)

  let set_element_rel hypercube comp v k =
    let open Rel_kind in
    let getter_and_setter_by_name name ss k =
      let open Compose in
      conde
        [
          name === r1 () &&& k ss.r1 (fun r1 -> { ss with r1 });
          name === r2 () &&& k ss.r2 (fun r2 -> { ss with r2 });
          name === r3 () &&& k ss.r3 (fun r3 -> { ss with r3 });
          name === r4 () &&& k ss.r4 (fun r4 -> { ss with r4 });
        ]
    in
    let get_index_by_id id k =
      hypercube.map
      |> List.map (fun (id0, index) ->
             id === inj_id id0 &&& delay (fun () -> k index))
      |> conde
    in

    fresh (id1 id2 name)
      (conde
         [
           name === r1 () &&& (comp === Rel.r1 id1 id2);
           name === r2 () &&& (comp === Rel.r2 id1 id2);
           name === r3 () &&& (comp === Rel.r3 id1 id2);
           name === r4 () &&& (comp === Rel.r4 id1 id2);
         ])
      (get_index_by_id id1 (fun index1 ->
           let ss = Array.get hypercube.cube index1 in
           getter_and_setter_by_name name ss (fun line setter ->
               get_index_by_id id2 (fun index2 ->
                   let new_cube = Array.copy hypercube.cube in
                   let new_line = Array.copy line in
                   Array.set new_line index2 v;
                   Array.set new_cube index1 (setter new_line);
                   delay (fun () -> k { hypercube with cube = new_cube })))))

  let fold_names hypercube f ~init =
    List.fold_left (fun acc (id1, _) -> f acc id1) init hypercube.map

  let gen_fold make_comp_rel f init hypercube =
    fold_names ~init hypercube (fun acc id1 ->
        List.fold_left
          (fun init comp ->
            fold_names ~init hypercube (fun acc id2 ->
                let r = get_element hypercube (Rel.make comp id1 id2) in
                f acc r (make_comp_rel comp id1 id2)))
          acc
          [ Rel_kind.R1; R2; R3; R4 ])

  let foldi :
        'acc 'value.
        ('acc -> 'value -> id Rel.ground -> 'acc) -> 'acc -> 'value t -> 'acc =
   fun eta -> gen_fold Rel.make eta

  let foldi_rel :
        'acc 'value.
        ('acc -> 'value ilogic -> id lkey -> 'acc) ->
        'acc ->
        'value ilogic t ->
        'acc =
   fun eta -> gen_fold (fun k a b -> Rel.make_rel inj_id !!k a b) eta

  let mapi f hypercube =
    let get_id index =
      fst @@ List.find (fun (_, i) -> i = index) hypercube.map
    in
    {
      hypercube with
      cube =
        Array.mapi
          (fun i1 ->
            let id1 = get_id i1 in
            Compose.mapi_cc (fun comp ->
                Array.mapi (fun i2 ->
                    let id2 = get_id i2 in
                    f @@ Rel.make comp id1 id2)))
          hypercube.cube;
    }

  (* let map f hypercube =
     let open Helpers in
     {
       hypercube with
       cube = Array.map (mapi_cc (fun _ -> Array.map f)) hypercube.cube;
     } *)

  let map_rel :
        'a 'b.
        ('a ilogic -> 'b ilogic -> goal) -> 'a ilogic t -> 'b ilogic t -> goal =
   fun f cube1 cube2 ->
    foldi_rel
      (fun acc e1 comp ->
        fresh e2 (get_element_rel cube2 comp e2) (f e1 e2) acc)
      success cube1

  (* For each element of hypercube we make list of compositions using relation `maker`.
     Result `rez` is a concatenation of these lists. *)
  let to_list :
        'value.
        'value ilogic t ->
        ('value ilogic -> iid Rel.injected -> iid comp_list -> goal) ->
        iid comp_list ->
        goal =
   fun hypercube maker rez ->
    let handler k e comp list =
      fresh (comps sublist) (maker e comp comps)
        (Std.List.appendo comps sublist list)
        (delay (fun () -> k sublist))
    in
    foldi_rel handler (( === ) (Std.nil ())) hypercube rez

  let hypercube_with_properties_to_list :
      iid Rel.injected Std.List.injected t -> iid comp_list -> goal =
   fun cube rez ->
    let open Std in
    let maker v comp comps = List.mapo (fun _ rez -> rez === comp) v comps in
    to_list cube maker rez
end

module Array_hypercube : S with type id = string = Array_hypercubeImpl

let comp_list_prj_exn : (_, 'a Rel.ground list) Reifier.t =
 fun x -> Std.List.prj_exn (Rel.prj_exn OCanren.prj_exn) x

let only_comp_list_show x =
  [%show: 'a Rel.ground Std.List.ground] (GT.lift Fun.id) () x

let%expect_test "Array_hypercube: fresh_hypercube and get_element" =
  let open Array_hypercube in
  let array_goal k = fresh_hypercube [ "a"; "b"; "c"; "d" ] k in
  let open Tester in
  let open GT in
  run_r reify
    (show logic @@ show bool)
    (-1) q qh
    ( "Array_hypercube: fresh_hypercube and get_element",
      fun q ->
        array_goal (fun cube ->
            fresh (r a) (a === !!"a")
              (get_element_rel cube (Rel.r1 a !!"b") q)
              (get_element_rel cube (Rel.r4 !!"c" !!"d") !!true)
              (get_element_rel cube (Rel.r4 !!"c" !!"d") r)
              (q === r)) );
  [%expect
    {|
    Array_hypercube: fresh_hypercube and get_element, all answers {
    q=true;
    }|}]

let%expect_test "Array_hypercube: foldi and to_list" =
  let open Array_hypercube in
  let open Tester in
  let test query ~msg =
    run_r comp_list_prj_exn only_comp_list_show (-1) q qh (msg, query)
  in

  test ~msg:"Array_hypercube: foldi and to_list" (fun q ->
      let set_only_r1 flag comp =
        conde
          [
            fresh (x y)
              (conde
                 [
                   comp === Rel.r2 x y; comp === Rel.r3 x y; comp === Rel.r4 x y;
                 ])
              (flag === !!false);
            fresh (x y) (comp === Rel.r1 x y) (flag === !!true);
          ]
      in
      fresh_hypercube [ "a"; "b"; "c"; "d" ] (fun cube ->
          foldi_rel (fun acc b comp -> acc &&& set_only_r1 b comp) success cube
          &&& to_list cube
                (fun e comp rez ->
                  conde
                    [
                      e === !!true &&& (rez === Std.(!<comp));
                      e === !!false &&& (rez === Std.nil ());
                    ])
                q));
  [%expect
    {|
    Array_hypercube: foldi and to_list, all answers {
    q=[R1 (d, d); R1 (d, c); R1 (d, b); R1 (d, a); R1 (c, d); R1 (c, c); R1 (c, b); R1 (c, a); R1 (b, d); R1 (b, c); R1 (b, b); R1 (b, a); R1 (a, d); R1 (a, c); R1 (a, b); R1 (a, a)];
    }|}]

let%expect_test "Array_hypercube: relational get element" =
  let open Array_hypercube in
  let open Tester in
  let test query ~msg =
    run_r comp_list_prj_exn only_comp_list_show (-1) q qh (msg, query)
  in

  let ids = [ "x5"; "x4"; "x3"; "x2"; "x1" ] in
  let query q =
    let open Std in
    fresh_hypercube ids (fun cube ->
        let in_lay comp = get_element_rel cube comp !!true in
        fresh ()
          (in_lay (Rel.r2 !!"x1" !!"x3"))
          (in_lay (Rel.r1 !!"x1" !!"x2"))
          (in_lay (Rel.r2 !!"x2" !!"x4"))
          (in_lay (Rel.r1 !!"x2" !!"x3"))
          (in_lay (Rel.r2 !!"x3" !!"x5"))
          (in_lay (Rel.r1 !!"x3" !!"x4"))
          (in_lay (Rel.r1 !!"x4" !!"x5"))
          (Array_hypercube.foldi_rel
             (fun acc b _ -> acc &&& condo2 (b === !!false) success)
             success cube)
          (to_list cube
             (fun e comp rez ->
               conde
                 [
                   e === !!true &&& (rez === Std.(!<comp));
                   e === !!false &&& (rez === Std.nil ());
                 ])
             q))
  in
  test query ~msg:"to list 2";
  [%expect
    {|
      to list 2, all answers {
      q=[R2 (x1, x3); R1 (x1, x2); R2 (x2, x4); R1 (x2, x3); R2 (x3, x5); R1 (x3, x4); R1 (x4, x5)];
      } |}];
  ()

let%expect_test "Array_hypercube: relational get element" =
  let open Array_hypercube in
  let array_goal k = fresh_hypercube [ "a"; "b" ] k in
  let query tested_comp q =
    array_goal (fun cube ->
        foldi_rel
          (fun acc b comp -> acc &&& (b === Std.( !< ) comp))
          success cube
        &&& get_element_rel cube tested_comp q)
  in
  let test query ~msg =
    let open Tester in
    let reify = Std.List.reify @@ Rel.reify reify in
    run_r reify
      ([%show: 'a logic Rel.logic Std.List.logic] (GT.lift Fun.id) ())
      (-1) q qh (msg, query)
  in
  test
    (fun q -> query (Rel.r3 (OCanren.inj "a") (OCanren.inj "b")) q)
    ~msg:"Array_hypercube: get element by ground argument";
  [%expect
    {|
      Array_hypercube: get element by ground argument, all answers {
      q=[R3 (a, b)];
      }|}];
  test
    (fun q -> fresh id (query (Rel.r2 (OCanren.inj "a") id) q))
    ~msg:"Array_hypercube: get element by almost ground argument";
  [%expect
    {|
      Array_hypercube: get element by almost ground argument, all answers {
      q=[R2 (a, a)];
      q=[R2 (a, b)];
      }|}];
  test
    (fun q -> fresh (id1 id2) (query (Rel.r1 id1 id2) q))
    ~msg:"Array_hypercube: get element by partly fresh argument";
  [%expect
    {|
      Array_hypercube: get element by partly fresh argument, all answers {
      q=[R1 (a, a)];
      q=[R1 (a, b)];
      q=[R1 (b, a)];
      q=[R1 (b, b)];
      }|}];
  test
    (fun q -> fresh p (query p q))
    ~msg:"Array_hypercube: get element by fully fresh argument";
  [%expect
    {|
      Array_hypercube: get element by fully fresh argument, all answers {
      q=[R1 (a, a)];
      q=[R1 (a, b)];
      q=[R1 (b, a)];
      q=[R1 (b, b)];
      q=[R2 (a, a)];
      q=[R2 (a, b)];
      q=[R2 (b, a)];
      q=[R2 (b, b)];
      q=[R3 (a, a)];
      q=[R3 (a, b)];
      q=[R3 (b, a)];
      q=[R3 (b, b)];
      q=[R4 (a, a)];
      q=[R4 (a, b)];
      q=[R4 (b, a)];
      q=[R4 (b, b)];
      }|}]

let%expect_test "Array_hypercube: relational set element" =
  let open Array_hypercube in
  let array_goal k = fresh_hypercube [ "a"; "b" ] k in
  let query tested_comp cube1 cube2 =
    let open Std in
    array_goal (fun cube ->
        foldi_rel (fun acc b _ -> acc &&& (b === nil ())) success cube
        &&& set_element_rel cube tested_comp !<tested_comp (fun new_cube ->
                hypercube_with_properties_to_list new_cube cube2)
        &&& hypercube_with_properties_to_list cube cube1)
  in
  let test query ~msg =
    let open Tester in
    run_r comp_list_prj_exn only_comp_list_show (-1) qr qrh (msg, query)
  in
  test
    (query (Rel.r4 !!"b" !!"a"))
    ~msg:"Array_hypercube: set element by ground argument";
  [%expect
    {|
      Array_hypercube: set element by ground argument, all answers {
      q=[]; r=[R4 (b, a)];
      }|}];
  test
    (fun c1 c2 -> fresh id (query (Rel.r1 id !!"b") c1 c2))
    ~msg:"Array_hypercube: set element by partly fresh argument";
  [%expect
    {|
      Array_hypercube: set element by partly fresh argument, all answers {
      q=[]; r=[R1 (a, b)];
      q=[]; r=[R1 (b, b)];
      }|}];
  test
    (fun c1 c2 -> fresh (id1 id2) (query (Rel.r2 id1 id2) c1 c2))
    ~msg:"Array_hypercube: set element by almost fresh argument";
  [%expect
    {|
      Array_hypercube: set element by almost fresh argument, all answers {
      q=[]; r=[R2 (a, a)];
      q=[]; r=[R2 (b, a)];
      q=[]; r=[R2 (a, b)];
      q=[]; r=[R2 (b, b)];
      }|}];
  test
    (fun c1 c2 -> fresh comp (query comp c1 c2))
    ~msg:"Array_hypercube: set element by fully ground argument";
  [%expect
    {|
    Array_hypercube: set element by fully ground argument, all answers {
    q=[]; r=[R1 (a, a)];
    q=[]; r=[R1 (b, a)];
    q=[]; r=[R1 (a, b)];
    q=[]; r=[R1 (b, b)];
    q=[]; r=[R2 (a, a)];
    q=[]; r=[R2 (b, a)];
    q=[]; r=[R2 (a, b)];
    q=[]; r=[R2 (b, b)];
    q=[]; r=[R3 (a, a)];
    q=[]; r=[R4 (a, a)];
    q=[]; r=[R3 (b, a)];
    q=[]; r=[R4 (b, a)];
    q=[]; r=[R3 (a, b)];
    q=[]; r=[R4 (a, b)];
    q=[]; r=[R3 (b, b)];
    q=[]; r=[R4 (b, b)];
    }|}]

let%expect_test "Non-relational test" =
  let h =
    Array_hypercube.init_hypercube [ "a"; "b"; "c" ] (function
      | Rel.Bin_rel (Rel_kind.R1, "a", "b") -> true
      | _ -> false)
  in
  let count h =
    Array_hypercube.foldi (fun acc flag _ -> if flag then acc + 1 else acc) 0 h
  in
  print_int (count h);
  [%expect {| 1 |}];
  let coh = Array_hypercube.mapi (fun _ -> not) h in
  print_int (count coh);
  [%expect {| 35 |}]
