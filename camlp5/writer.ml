type 'a t = string list * 'a

let return x = [],x

let run = Fun.id
let (<*>) (vars1, f) (vars2, x) = (vars1 @ vars2, f x)

let (>>|) (xs, x) f = (xs, f x)

let write (vars, r) s = (s::vars, r)
(*
  let (>>=) (vars1, x) f =
    let vars2,r = f x in
    (vars1 @ vars2, r)

  let ( let* ) = (>>=)

  let (let+) (vars,x) f = (vars, f x)
  let (and+) (xs, l) (ys, r) = (xs@ys, (l,r))

  let foldlm f i xs =
    let* xs = xs in
    List.fold_left  (fun acc x -> let* acc = acc in f acc x) i xs

  let foldrm f xs i =
    let* xs = xs in
    List.fold_right (fun x acc -> let* acc = acc in f x acc) xs i

  let mapm f xs =
    List.fold_right (fun x acc -> (return List.cons) <*> (f x) <*> acc) xs (return [])

  let fold_right1m_exn f xs =
    xs >>= function
    | [] -> failwith "Bad argument"
    | h::tl -> foldrm f (return tl) (return h)
*)    
