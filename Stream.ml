open Printf

type 'a t = ('a * 'a t) Lazy.t

exception End_of_stream

let from_fun (f: unit -> 'a t) : 'a t =
  Lazy.lazy_from_fun (fun () -> Lazy.force (f ()))

let nil        = from_fun (fun () -> raise End_of_stream)

let cons h t : 'a t  = Lazy.lazy_from_val (h, t)

let is_empty (s: 'a t) =
  try ignore (Lazy.force s); false with End_of_stream -> true

let hd (s: 'a t) = fst (Lazy.force s)

let tl (s: 'a t) = snd (Lazy.force s)

let destruct (s: 'a t) =
  try `Cons (Lazy.force s) with End_of_stream -> `Nil

let rec concat s1 s2 =
  from_fun (
    fun () ->
      match destruct s1 with
      | `Nil -> s2
      | `Cons (h, t) -> cons h (concat t s2)
  )

let rec foldl f acc s =
  printf "Stream.foldl\n%!";
  match destruct s with
  | `Nil -> acc
  | `Cons (x, xs) -> foldl f (f acc x) xs

let rec map f s =
  from_fun (
    fun () ->
      match destruct s with
      | `Cons (x, xs) -> cons (f x) (map f xs)
      | `Nil -> nil
  )

let take n s =
  let rec inner i s =
    if i = 0
    then []
    else
      let () = printf "requesting element on pos %d\n%!" (n-i) in
      match destruct s with
      | `Nil -> []
      | `Cons (x, xs) -> x :: inner (i-1) xs
  in
  inner n s

let take_all s = take (-1) s


(*
#
let r =
  let a = from_fun (fun () -> print_endline "eval 1"; cons 1 nil) in
  let b = from_fun (fun () -> print_endline "eval 100"; cons 100 nil) in
  concat a b
;;

val r : int t = <lazy>
let f x =
  let a = from_fun (fun () -> let n = x+2 in printf "eval %d\n%!" n; cons n nil) in
  let b = from_fun (fun () -> let n = x*2 in printf "eval %d\n%!" n; cons n nil) in
  concat a b
;;

val f : int -> int t = <fun>
#
let ans  = from_fun (fun () -> foldl concat nil (map f r));;

val ans : int t = <lazy>
#   take 1 ans;;
requesting element on pos 0
Stream.foldl
eval 1
Stream.foldl
eval 100
Stream.foldl
eval 3
- : int list = [3]
#

*)
