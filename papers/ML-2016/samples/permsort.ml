open GT
open MiniKanren 

@type 'self nat = Z | S of 'self with gmap, show

type lnat = lnat nat logic
type gnat = gnat nat

let (!) = (!!)

let rec inj_nat (*: gnat -> lnat*) = fun n -> !(gmap(nat) inj_nat n)
let rec prj_nat (*: lnat -> gnat*) = fun n -> gmap(nat) prj_nat (prj n)

let rec nat_of_int = function 0 -> Z | n -> S (nat_of_int (n-1))
let rec int_of_nat = function Z -> 0 | S n -> 1 + int_of_nat n

;;

@type ('a, 'self) list = Cons of 'a * 'self | Nil with gmap, show

let rec of_list = function
| [] -> Nil
| x::xs -> Cons (x, of_list xs)

let rec to_list = function
| Nil -> []
| Cons (x, xs) -> x :: to_list xs

type 'a glist = ('a, 'a glist) list
type 'a llist = ('a, 'a llist) list logic

let rec inj_list (*: 'a glist -> 'a llist*) = fun l ->
  inj (gmap(list) (fun x -> x) inj_list l)

let rec prj_list (*: 'a llist -> 'a glist*) = fun l ->
  gmap(list) (fun x -> x) prj_list (prj l)

let rec leo x y =
  conde [
    x === !Z;
    fresh (x' y') 
      (x === !(S x'))
      (y === !(S y'))
      (leo x' y')
  ]

let rec sorto x y =
  let rec inserto x y xy =
    conde [
      (y === !Nil) &&& (xy === !(Cons (x, !Nil)));
      fresh (hd tl)
        (y === !(Cons (hd, tl)))
        (conde [
           (leo x hd) &&& (xy === !(Cons (x, y)));
           fresh (xy')
             (leo hd x)
             (xy === !(Cons (hd, xy'))) 
             (inserto x tl xy')
         ]
        )
    ]
  in
  conde [
    (x === !Nil) &&& (y === !Nil);
    fresh (hd tl tl') 
      (x === !(Cons (hd, tl)))
      (inserto hd tl' y)
      (sorto tl tl')
  ]

let inj_int_list (*: int GT.list -> lnat llist *) = fun l -> inj_list @@ of_list (List.map (fun n -> inj_nat (nat_of_int n)) l)
let prj_int_list (*: lnat llist -> int GT.list *) = fun l -> List.map (fun n -> int_of_nat @@ prj_nat n) @@ to_list (prj_list l)

let perm l =
  let rec fact = function
  | 0 -> 1
  | n -> n * fact (n-1)
  in
  run q (fun q -> sorto q (inj_int_list (List.sort Pervasives.compare l)))
        (fun a -> List.map prj_int_list (Stream.take ~n:(fact @@ List.length l) a))

let _ =
  List.iter (fun l -> Printf.printf "%s\n" (show(GT.list) (show(GT.int)) l)) (perm [1; 2; 3; 4; 5])

