open GT
open MiniKanren
open Tester
open Printf

(* let just_a a = a === (5 |> lift |> inj) *)

(* let rec show_list l = show(llist) (show(int)) show_list l;; *)
(*
@type 'a test = A of 'a with show;;

module LTest = Fmap (struct type 'a t = 'a test let fmap f = function A x -> A (f x)  end)
module LOption = Fmap (struct
  type 'a t = 'a option
  let fmap f = function Some x -> Some(f x) | None -> None
end)

let _ =
  let () = MiniKanren.run q
    (fun q -> inj (LTest.fmap (A q)) === inj (LTest.fmap (A (inj (lift 5)))))
    (fun qs -> printf "%s\n" (show(int) @@ Stream.hd qs))
  in

  let (_: (int, int logic) fancy) = inj @@ lift 5 in
  let (_: (int option, int logic option) fancy) = LOption.fmap (Some (inj @@ lift 5)) in
  let () =
    MiniKanren.run q
      (fun q -> q === inj (LOption.fmap (Some (inj (lift 5)))))
      (fun qs -> printf "%s\n" (show(option) (show(int)) @@ Stream.hd qs))
  in *)
(*
  run show_int_list  1  q (REPR (fun q   -> appendo q (inj_list [3; 4]) (inj_list [1; 2; 3; 4]))) qh;
  run show_int_list  4 qr (REPR (fun q r -> appendo q (inj_list []) r                          )) qrh;
  run show_int_list  1  q (REPR (fun q   -> reverso q (inj_list [1; 2; 3; 4])                  )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list []) (inj_list [])                )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1; 2; 3; 4]) q                  )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  3  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list 10  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q (inj_list [1])                           )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1]) q                           )) qh;
  run show_int       1  q (REPR (fun q   -> a_and_b q                                          )) qh;
  run show_int       2  q (REPR (fun q   -> a_and_b' q                                         )) qh;
  run show_int      10  q (REPR (fun q   -> fives q                                            )) qh
*)
  (* ()
;; *)

@type 'a maybe =
  | Nothing
  (* | Dummy1 of int *)
  (* | Dummy2 of string *)
  (* | Dummy3 of string list *)
  (* | Dummy4 of ('a list) *)
  (* | Dummy5 of int*int *)
  (* | Dummy6 of int*int*string *)
  | Just of 'a  with show;;

module Maybe = Fmap1 (struct
  type 'a t = 'a maybe
  let fmap f x =
    let () = printf "Maybe.fmap of '%s'\n%!" (generic_show x) in
    match x with
    | Just a  -> print_endline "Got    Just"; Just (f a)
    | Nothing -> print_endline "Got Nothing"; Nothing
    | _ -> assert false
end)

let show_logic_maybe f x : string = show unlogic (show(maybe) f) x

(* let (_: int) = (inj @@ Maybe.fmap @@ (Just (inj@@lift 15)) ) *)
(* let (_:int) = (===)
let (_:_ fancy -> goal) = fun q ->
  let (right: (int maybe, int logic maybe logic,
               int unlogic maybe unlogic) fancy)
    = inj @@ Maybe.fmap @@ (Just (inj@@lift 15))  in
  q === right
let (_:int) = MiniKanren.run q *)

let _flat_int () =
  print_endline " ---------------------------------------------- flat_int";
  MiniKanren.run q (fun q ->
    let (right: (int, int  logic, int unlogic) fancy)
      = inj@@lift 15 in

    printf "hack is '%s'\n%!" (generic_show @@ (snd @@ Obj.magic right) (fun _ -> false) 15);
    q === right)
  (fun qs ->
    let () = printf "head is '%s'\n%!" (generic_show @@ Stream.hd qs) in
    printf "%s\n" @@ (show unlogic @@ show int) @@ Stream.hd qs)
;;

let _just_of_int () =
  print_endline " ---------------------------------------------- just_of_int";
  MiniKanren.run q (fun q ->
    let (right: (int maybe, int logic maybe logic,
                 int unlogic maybe unlogic) fancy)
      = inj @@ Maybe.fmap @@ (Just (inj@@lift 15)) in
    q === right)
  (fun qs ->
    (* let () = printf "head is '%s'\n%!" (generic_show @@ Stream.hd qs) in *)
    printf "%s\n" @@ show_logic_maybe (show(unlogic)@@show(int)) @@ Stream.hd qs)
;;

let _just_2 () =
  print_endline " ---------------------------------------------- _just_2";
  MiniKanren.run q (fun q ->
    let (right: (int maybe, int logic maybe logic,
                 int unlogic maybe unlogic) fancy)
      = inj @@ Maybe.fmap @@ (Just (inj@@lift 15)) in
    (inj @@ Maybe.fmap @@ (Just q)) === right)
  (fun qs ->
    (* let () = printf "head is '%s'\n%!" (generic_show @@ Stream.hd qs) in *)
    printf "%s\n" @@ (show(unlogic)@@show(int)) @@ Stream.hd qs)
;;


(* let () =
  MiniKanren.run q
    (fun q ->
      let () = printf "+++++++++++++++++++++++++++\n%!" in
      let (right: (int, int logic , int unlogic ) fancy)  = inj@@lift 15 in
      q === right)
    (fun qs -> printf "Answer: %s\n" ((show(unlogic)@@show(int)) @@ Stream.hd qs))
;; *)

let _asdf () =
  MiniKanren.run q (fun q ->
    let () =
      (* let s,x = REPR(Just 15) in
      let () = printf "%30s                              is       %s\n\n%!" s (generic_show x) in *)
      (* let s,x = REPR(q) in
      let () = printf "%30s                              is       %s\n\n%!" s (generic_show x) in
      let s,x = REPR(inj(lift 15)) in
      let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in
      let s,x = REPR(Just (inj(lift 15))) in
      let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in
      let s,x = REPR(Maybe.fmap (Just (inj(lift 15)))) in
      let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in
      let s,x = REPR(inj (Maybe.fmap (Just (inj(lift 15))))) in
      let () = printf "%30s                             is       %s\n%!" s (generic_show x) in *)
      (* let s,x = REPR(Value(Just(Value 15))) in
      let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in *)
      ()
    in
    let (right: (int maybe, int logic maybe logic,
                 int unlogic maybe unlogic) fancy)
      = inj @@ Maybe.fmap @@ (Just (inj@@lift 15)) in
    q === right)
  (fun qs ->
    (* let () = printf "head is '%s'\n%!" (generic_show @@ Stream.hd qs) in *)
    printf "%s\n" (show_logic_maybe (show(unlogic)@@show(int)) @@ Stream.hd qs))
;;

(* let rec show_test t = show(test) (show(int)) t *)
@type ('a,'b) result = OK of 'a | Error of 'b with show
module Result = Fmap2(struct
  type ('a,'b) t = ('a,'b) result
  let fmap f g = function OK a -> OK (f a) | Error b -> Error (g b)
end);;

let show_logic_result f g x : string = show unlogic (show(result) f g) x

let test_result () =
  MiniKanren.run q (fun q ->
    let (right: ((int,string) result, (int logic,string logic) result logic,
                 (int unlogic, string unlogic) result unlogic) fancy)
      = inj @@ Result.fmap @@ (OK (inj@@lift 15)) in
    q === right)
  (fun qs ->
    (* let () = printf "head is '%s'\n%!" (generic_show @@ Stream.hd qs) in *)
    printf "%s\n" (show_logic_result (show(unlogic)@@show(int)) (show unlogic @@ show string) @@ Stream.hd qs));
;;



(* Lists ******************************* *)
@type ('a, 'b) alist = Nil | Cons of 'a * 'b with show;;
type int_list = (int, int_list) alist
type int_llist = (int logic, int_llist logic) alist


module F = Fmap2 (struct
  type ('a, 'b) t = ('a, 'b) alist
  let fmap f g = function
  | Nil -> Nil
  | Cons (x, y) -> Cons (f x, g y)
end)

let nil ()  = inj (F.fmap Nil)
let cons x y = inj (F.fmap (Cons (x, y)))

let rec show_intlist xs = show(alist) (show(int)) show_intlist xs
let (_: ((int, 'a) alist as 'a) -> bytes) = show_intlist
let rec show_alist_logic xs =
  show(unlogic) (show(alist) (show(unlogic) @@ show(int)) show_alist_logic) xs

let test1 x =
  Fresh.one (fun y ->
    (* (x === cons (inj@@lift 5) y) &&& *)
    (x === y) &&&
    (y === nil ())
  )

let () =
    MiniKanren.run q test1
      (fun qs ->
        (* let (_:int) = qs in         *)
        let first = Stream.hd qs in

        printf "Answer: %s\n" (show_alist_logic first))
  ;;
