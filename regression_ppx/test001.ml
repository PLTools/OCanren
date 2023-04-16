open OCanren
open Tester

module _ = struct
  [%%distrib
  type nonrec 'a t =
    | Z
    | S of 'a
  [@@deriving gt ~options:{ gmap; show }]

  type ground = ground t]

  let run_peano_exn n = run_r prj_exn (GT.show ground) n
  let run_peano n = run_r reify (GT.show logic) n

  let () =
    run_peano 1 q qh (REPR (fun q -> q === z ()));
    run_peano 1 q qh (REPR (fun q -> q === s (z ())))
  ;;
end

module _ = struct
  [%%distrib
  type nonrec 'a t =
    | None
    | Some of 'a
  [@@deriving gt ~options:{ gmap; show }]

  type nonrec 'a ground = 'a t]

  let run_option n =
    run_r [%reify: GT.int ground] (GT.show logic (GT.show OCanren.logic (GT.show GT.int))) n
  ;;

  let () =
    run_option 1 q qh (REPR (fun q -> q === none ()));
    run_option 1 q qh (REPR (fun q -> fresh x (q === some x)));
    run_option 1 q qh (REPR (fun q -> fresh () (q === some !!42)))
  ;;
end

module _ = struct
  [%%distrib
  type nonrec ('a, 'b) t =
    | [] [@name "nil"]
    | ( :: ) of 'a * 'b [@name "cons"]
  [@@deriving gt ~options:{ gmap; show }]

  type 'a ground = ('a, 'a ground) t]

  let __ : ('a -> string) -> ('a logic as 'b) -> string = GT.show logic

  let __ : int OCanren.logic logic -> string =
    GT.show logic (GT.show OCanren.logic (GT.show GT.int))
  ;;

  let __ : ((('b ilogic, 'a) t ilogic as 'a), (('b, 'c) t as 'c)) OCanren__Logic.Reifier.t =
    prj_exn OCanren.prj_exn
  ;;

  let __ = reify OCanren.reify

  let run_list n =
    run_r [%reify: GT.int t] (GT.show logic (GT.show OCanren.logic (GT.show GT.int))) n
  ;;

  let () =
    run_list 1 q qh (REPR (fun q -> q === nil ()));
    run_list 1 q qh (REPR (fun q -> fresh x (q === cons x (nil ()))))
  ;;
end

module Moves = struct
  [%%distrib
  type nonrec 'nat t =
    | Forward of 'nat
    | Backward of 'nat
    | Unload of 'nat
    | Fill of 'nat
  [@@deriving gt ~options:{ gmap; show }]

  type nonrec ground = GT.int t]
end

module _ = struct
  [%%distrib
  type nonrec ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving gt ~options:{ gmap; show }]
  type nonrec ('a, 'b, 'c) ground = ('a, 'b, 'c) t]
end
