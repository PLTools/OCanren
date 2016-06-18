open GT
open MiniKanren

(* Abstracted type for the tree *)
@type ('a, 'self) tree = Nil | Node of 'a * 'self * 'self with gmap, show

(* A shortcut for "ground" tree we're going to work with in "functional" code *)
type gtree = (int, gtree) tree

(* Logic counterpart *)
type ltree = (Nat.logic, ltree) tree logic

(* Printing ground tree function *)
let rec show_tree t = show(tree) (show(int)) show_tree t

(* Injection *)
let rec inj_tree : gtree -> ltree = fun t ->
  !! (gmap(tree) inj_nat inj_tree t)

(* Projection *)
let rec prj_tree : ltree -> gtree = fun t ->
  gmap(tree) prj_nat prj_tree (prj t)

(* Relational insert into a search tree *)
let rec inserto a t t' = conde [
  (t === !!Nil) &&& (t' === !!(Node (a, !!Nil, !!Nil)));
  fresh (x l r l')
    (t === !!(Node (x, l, r)))
    Nat.(conde [
      (t' === t) &&& (a === x);
      (t' === !!(Node (x, l', r ))) &&& (a < x) &&& (inserto a l l');
      (t' === !!(Node (x, l , l'))) &&& (a > x) &&& (inserto a r l')
    ])
]

(* Top-level wrapper for insertion --- takes and returns non-logic data *)
let insert a t =
  run q (fun q  -> inserto (inj_nat a) (inj_tree t) q)
        (fun qs -> prj_tree @@ Stream.hd qs)

(* Top-level wrapper for "inverse" insertion --- returns an integer, which
   has to be inserted to convert t into t' *)
let insert' t t' =
  run q (fun q  -> inserto q (inj_tree t) (inj_tree t'))
        (fun qs -> prj_nat @@ Stream.hd qs)  

(* Entry point *)
let _ =
  let insert_list l =
    let rec inner t = function 
    | []    -> t
    | x::xs -> 
       let t' = insert x t in
       Printf.printf "Inserting %d into %s makes %s\n%!" x (show_tree t) (show_tree t');
       inner t' xs
    in
    inner Nil l
  in
  ignore @@ insert_list [1; 2; 3; 4];
  let t  = insert_list [3; 2; 4; 1] in
  let t' = insert 8 t in
  Printf.printf "Inverse insert: %d\n" @@ insert' t t'
