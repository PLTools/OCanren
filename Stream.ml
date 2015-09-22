type 'a t = ('a * 'a t) Lazy.t

exception End_of_stream

let from_fun f =
  Lazy.lazy_from_fun (fun () -> Lazy.force (f ()))

let nil        = from_fun (fun () -> raise End_of_stream)

let cons h t   = Lazy.lazy_from_val (h, t)

let is_empty s = 
  try ignore (Lazy.force s); false with End_of_stream -> true
	  
let hd s = fst (Lazy.force s)

let tl s = snd (Lazy.force s)

let destruct s =
  try `Cons (Lazy.force s) with End_of_stream -> `Nil
	  
let rec concat s1 s2 = 
  from_fun (
    fun () -> 
      match destruct s1 with
      | `Nil -> s2
      | `Cons (h, t) -> cons h (concat t s2)
  )

let rec foldl f y s = 
  match destruct s with
  | `Nil -> y
  | `Cons (x, xs) -> foldl f (f y x) xs
      
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
    else match destruct s with
    | `Nil -> []
    | `Cons (x, xs) -> x :: inner (i-1) xs
  in
  inner n s
    
let take_all s = take (-1) s
