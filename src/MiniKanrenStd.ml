(*
 * MiniKanrenStd: miniKanren standard library implementation.
 * Copyright (C) 2015-2017
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
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

open MiniKanrenCore

@type ('a, 'l) list = Nil | Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr;;
@type 'a nat = O | S of 'a with show, html, eq, compare, foldl, foldr, gmap;;

module Pair = 
  struct

    type 'a logic' = 'a logic

    let logic' = logic

    type ('a, 'b) ground = 'a * 'b

    let ground = GT.pair

    type ('a, 'b) logic  = ('a * 'b) logic'

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    f g n   = GT.html   (GT.pair) f g n
          method eq      f g n m = GT.eq     (GT.pair) f g n m
          method compare f g n m = GT.compare(GT.pair) f g n m
          method foldr   f g a n = GT.foldr  (GT.pair) f g a n
          method foldl   f g a n = GT.foldl  (GT.pair) f g a n
          method gmap    f g n   = GT.gmap   (GT.pair) f g n
          method show    f g n   = GT.show   (GT.pair) f g n
        end
    }

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    f g n   = GT.html   (logic') (GT.html   (ground) f g) n
          method eq      f g n m = GT.eq     (logic') (GT.eq     (ground) f g) n m
          method compare f g n m = GT.compare(logic') (GT.compare(ground) f g) n m
          method foldr   f g a n = GT.foldr  (logic') (GT.foldr  (ground) f g) a n
          method foldl   f g a n = GT.foldl  (logic') (GT.foldl  (ground) f g) a n
          method gmap    f g n   = GT.gmap   (logic') (GT.gmap   (ground) f g) n
          method show    f g n   = GT.show   (logic') (GT.show   (ground) f g) n
        end
    }

    let inj f g p = to_logic (GT.(gmap pair) f g p)

    type ('a, 'b, 'c, 'd) groundi = (('a, 'c) ground, ('b, 'd) logic) injected

    module T =
      struct
        type ('a, 'b) t = 'a * 'b 
        let fmap f g x = GT.(gmap pair) f g x
      end

    include T
    include Fmap2 (T)

    let pair x y = MiniKanrenCore.inj @@ distrib (x, y)

  end

module Option = 
  struct

    type 'a logic' = 'a logic

    let logic' = logic

    type 'a ground = 'a option

    let ground = GT.option

    type 'a logic  = 'a option logic'

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (GT.option) n
          method eq      n m = GT.eq     (GT.option) n m
          method compare n m = GT.compare(GT.option) n m
          method foldr   n   = GT.foldr  (GT.option) n
          method foldl   n   = GT.foldl  (GT.option) n
          method gmap    n   = GT.gmap   (GT.option) n
          method show    n   = GT.show   (GT.option) n
        end
    }

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    f n   = GT.html   (logic') (GT.html   (ground) f) n
          method eq      f n m = GT.eq     (logic') (GT.eq     (ground) f) n m
          method compare f n m = GT.compare(logic') (GT.compare(ground) f) n m
          method foldr   f a n = GT.foldr  (logic') (GT.foldr  (ground) f) a n
          method foldl   f a n = GT.foldl  (logic') (GT.foldl  (ground) f) a n
          method gmap    f n   = GT.gmap   (logic') (GT.gmap   (ground) f) n
          method show    f n   = GT.show   (logic') (GT.show   (ground) f) n
        end
    }

    let inj f x = to_logic (GT.(gmap option) f x)

    type ('a, 'b) groundi = ('a ground, 'b logic) injected

    module T =
      struct
        type 'a t = 'a option
        let fmap f x = GT.(gmap option) f x
      end

    include T
    include Fmap (T)

    let some x  = MiniKanrenCore.inj @@ distrib (Some x)
    let none () = MiniKanrenCore.inj @@ distrib None

    let option = function None -> none () | Some x -> some x

  end

module Bool =
  struct

    type 'a logic' = 'a logic

    let logic' = logic

    type ground = bool
    type t      = bool

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (GT.bool) n
          method eq      n m = GT.eq     (GT.bool) n m
          method compare n m = GT.compare(GT.bool) n m
          method foldr   n   = GT.foldr  (GT.bool) n
          method foldl   n   = GT.foldl  (GT.bool) n
          method gmap    n   = GT.gmap   (GT.bool) n
          method show    n   = GT.show   (GT.bool) n
        end
    }

    type logic = bool logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (logic') (GT.html   (ground)) n
          method eq      n m = GT.eq     (logic') (GT.eq     (ground)) n m
          method compare n m = GT.compare(logic') (GT.compare(ground)) n m
          method foldr   a n = GT.foldr  (logic') (GT.foldr  (ground)) a n
          method foldl   a n = GT.foldl  (logic') (GT.foldl  (ground)) a n
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (ground)) n
          method show    n   = GT.show   (logic') (GT.show   (ground)) n
        end
    }

    let inj = to_logic

    type groundi = (ground, logic) injected

    let reify = MiniKanrenCore.reify

    let falso = MiniKanrenCore.inj @@ lift false
    let truo  = MiniKanrenCore.inj @@ lift true

    let (|^) a b c =
      conde [
        (a === falso) &&& (b === falso) &&& (c === truo );
        (a === falso) &&& (b === truo ) &&& (c === truo );
        (a === truo ) &&& (b === falso) &&& (c === truo );
        (a === truo ) &&& (b === truo ) &&& (c === falso);
      ]

    let noto a na = (a |^ a) na

    let oro a b c =
      Fresh.two (fun aa bb ->
        ((a  |^ a) aa) &&&
        ((b  |^ b) bb) &&&
        ((aa |^ bb) c)
      )

    let ando a b c =
      Fresh.one (fun ab ->
        ((a  |^ b) ab) &&&
        ((ab |^ ab) c)
      )

    let (~~) a   = noto a truo
    let (&&) a b = ando a b truo
    let (||) a b = oro  a b truo

  end

let eqo x y t =
  conde [
    (x === y) &&& (t === Bool.truo);
    (x =/= y) &&& (t === Bool.falso);
  ]

let neqo x y t =
  conde [
    (x =/= y) &&& (t === Bool.truo);
    (x === y) &&& (t === Bool.falso);
  ]

module Nat = 
  struct

    type 'a logic' = 'a logic
    let logic' = logic

    module X = 
      struct
        type 'a t = 'a nat
        let fmap f x = GT.(gmap nat) f x 
      end

    include X

    module F = Fmap (X)

    type ground = ground t
    type logic = logic t logic'
    type groundi = (ground, logic) injected

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n = GT.html   (nat) this#html    n
          method eq      n = GT.eq     (nat) this#eq      n
          method compare n = GT.compare(nat) this#compare n
          method foldr   n = GT.foldr  (nat) this#foldr   n
          method foldl   n = GT.foldl  (nat) this#foldl   n
          method gmap    n = GT.gmap   (nat) this#gmap    n
          method show    n = GT.show   (nat) this#show    n
        end
    }

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (logic') (GT.html   (nat) this#html   ) n
          method eq      n m = GT.eq     (logic') (GT.eq     (nat) this#eq     ) n m
          method compare n m = GT.compare(logic') (GT.compare(nat) this#compare) n m
          method foldr   a n = GT.foldr  (logic') (GT.foldr  (nat) this#foldr  ) a n
          method foldl   a n = GT.foldl  (logic') (GT.foldl  (nat) this#foldl  ) a n
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (nat) this#gmap   ) n
          method show    n   = GT.show   (logic') (GT.show   (nat) this#show   ) n
        end
    }

    let rec of_int n = if n <= 0 then O else S (of_int (n-1))
    let rec to_int   = function O -> 0 | S n -> 1 + to_int n

    let rec inj n = to_logic (GT.(gmap nat) inj n)

    let rec reify h n = F.reify reify h n

    let o   = MiniKanrenCore.inj @@ F.distrib O
    let s x = MiniKanrenCore.inj @@ F.distrib (S x)

    let rec nat n = MiniKanrenCore.inj @@ F.distrib @@ X.fmap nat n

    let zero = o
    let one  = s o
    let succ = s

    let rec addo x y z =
      conde [
        (x === o) &&& (z === y);
        Fresh.two (fun x' z' ->
           (x === (s x')) &&&
           (z === (s z')) &&&
           (addo x' y z')
        )
      ]

    let (+) = addo

    let rec mulo x y z =
      conde
        [ (x === o) &&& (z === o)
        ; Fresh.two (fun x' z' ->
            (x === (s x')) &&&
            (addo y z' z) &&&
            (mulo x' y z')
          )
        ]

    let ( * ) = mulo

    let rec leo x y b =
      conde [
        (x === o) &&& (b === Bool.truo);
        (x =/= o) &&& (y === o) &&& (b === Bool.falso);
        Fresh.two (fun x' y' ->
          (x === (s x')) &&& (y === (s y')) &&& (leo x' y' b)
        )
      ]

    let geo x y b = leo y x b

    let (<=) x y = leo x y Bool.truo
    let (>=) x y = geo x y Bool.truo

    let rec gto x y b = conde
      [ (x =/= o) &&& (y === o) &&& (b === Bool.truo)
      ; (x === o) &&& (b === Bool.falso)
      ; Fresh.two (fun x' y' ->
          (x === s x') &&& (y === s y') &&& (gto x' y' b)
        )
      ]

    let lto x y b = gto y x b

    let (>) x y = gto x y Bool.truo
    let (<) x y = lto x y Bool.truo

  end

let nat n = Nat.nat (Nat.of_int n) 

module List =
  struct

    include List

    type 'a logic' = 'a logic

    let logic' = logic

    type ('a, 'l) t = ('a, 'l) list

    module X = 
      struct
        type ('a,'b) t = ('a, 'b) list
        let fmap f g x = GT.gmap list f g x
      end

    module F = Fmap2 (X)

    let nil ()    = MiniKanrenCore.inj @@ F.distrib Nil
    let conso x y = MiniKanrenCore.inj @@ F.distrib (Cons (x, y))

    type 'a ground = ('a, 'a ground) t
    type 'a logic  = ('a, 'a logic) t logic'

    let rec reify r1 h = F.reify r1 (reify r1) h

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    fa l = GT.html   (list) fa (this#html    fa) l
          method eq      fa l = GT.eq     (list) fa (this#eq      fa) l
          method compare fa l = GT.compare(list) fa (this#compare fa) l
          method foldr   fa l = GT.foldr  (list) fa (this#foldr   fa) l
          method foldl   fa l = GT.foldl  (list) fa (this#foldl   fa) l
          method gmap    fa l = GT.gmap   (list) fa (this#gmap    fa) l
          method show    fa l = "[" ^
            let rec inner l =
              (GT.transform(list)
                 (GT.lift fa)
                 (GT.lift inner)
                 (object inherit ['a,'a ground] @list[show]
                    method c_Nil   _ _      = ""
                    method c_Cons  i s x xs = x.GT.fx () ^ (match xs.GT.x with Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                  end)
                 ()
                 l
              )
            in inner l ^ "]"
        end
    }

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method compare fa l = GT.compare (logic') (GT.compare (list) fa (this#compare fa)) l
          method gmap    fa l = GT.gmap    (logic') (GT.gmap    (list) fa (this#gmap    fa)) l
          method eq      fa l = GT.eq      (logic') (GT.eq      (list) fa (this#eq      fa)) l
          method foldl   fa l = GT.foldl   (logic') (GT.foldl   (list) fa (this#foldl   fa)) l
          method foldr   fa l = GT.foldr   (logic') (GT.foldr   (list) fa (this#foldr   fa)) l
          method html    fa l = GT.html    (logic') (GT.html    (list) fa (this#html    fa)) l
          method show : ('a -> string) -> 'a logic -> GT.string = fun fa l ->
            GT.show(logic')
              (fun l -> "[" ^
                 let rec inner l =
                    GT.transform(list)
                      (GT.lift fa)
                      (GT.lift (GT.show(logic) inner))
                      (object inherit ['a,'a logic] @list[show]
                         method c_Nil   _ _      = ""
                         method c_Cons  i s x xs =
                           x.GT.fx () ^ (match xs.GT.x with Value Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                       end)

                    () l
                   in inner l ^ "]"
              )
              l
        end
    }

    let rec of_list f = function
    | []    -> Nil
    | x::xs -> Cons (f x, of_list f xs)

    let rec to_list f = function
    | Nil -> []
    | Cons (x,xs) -> f x :: to_list f xs

    let rec inj f xs = to_logic (GT.gmap list f (inj f) xs)

    let rec list = function
    | []    -> nil ()
    | x::xs -> conso x (list xs)

    type ('a,'b) groundi = ('a ground, 'b logic) injected

    let groundi =
      { GT.gcata = ()
      ; plugins = object
          method show : ('a -> string) -> ('a,_) groundi -> string = fun fa l ->
          GT.show(ground) fa (Obj.magic l : 'a ground)
        end
      }

    let (%): ('a,'b) injected -> ('a,'b) groundi -> ('a,'b) groundi = conso
    let (%<): ('a,'b) injected -> ('a,'b) injected -> ('a,'b) groundi = fun x y -> conso x @@ conso y @@ nil ()
    let (!<) : ('a,'b) injected -> ('a,'b) groundi = fun x -> conso x @@ nil ()

    let rec foldro f a xs r =
      conde [
        (xs === nil ()) &&& (a === r);
        Fresh.three (fun h t a'->
            (xs === h % t) &&&
            (f h a' r) &&&
            (foldro f a t a')
        )
      ]

    let rec mapo f xs ys =
      conde [
        (xs === nil ()) &&& (ys === nil ());
        Fresh.two (fun z zs ->
          (xs === z % zs) &&&
          (Fresh.two (fun a1 a2 ->
              (f z a1) &&&
              (mapo f zs a2) &&&
              (ys === a1 % a2)
          ))
        )
      ]

    let filtero p xs ys =
      let folder x a a' =
        conde [
          (p x Bool.truo) &&& (x % a === a');
          (p x Bool.falso) &&& (a === a')
        ]
      in
      foldro folder (nil ()) xs ys

    let rec lookupo p xs mx =
      conde [
        (xs === nil ()) &&& (mx === Option.none ());
        Fresh.two (fun h t ->
          (h % t === xs) &&&
          (conde [
            (p h Bool.truo) &&& (mx === (Option.some h));
            (p h Bool.falso) &&& (lookupo p t mx)
          ])
        )
      ]

    let anyo = foldro Bool.oro Bool.falso

    let allo = foldro Bool.ando Bool.truo

    let rec lengtho l n =
      conde [
        (l === nil ()) &&& (n === Nat.o);
        Fresh.three (fun x xs n' ->
          (l === x % xs)  &&&
          (n === (Nat.s n')) &&&
          (lengtho xs n')
        )
      ]

    let rec appendo a b ab =
      conde [
        (a === nil ()) &&& (b === ab);
        Fresh.three (fun h t ab' ->
          (a === h%t) &&&
          (h%ab' === ab) &&&
          (appendo t b ab')
        )
      ]

    let rec reverso a b =
      conde [
        (a === nil ()) &&& (b === nil ());
        Fresh.three (fun h t a' ->
          (a === h%t) &&&
          (appendo a' !<h b) &&&
          (reverso t a')
        )
      ]

    let rec membero (l: (_,_) groundi) a =
      Fresh.two (fun x xs ->
        (l === x % xs) &&&
        (conde [
          x === a;
          (x =/= a) &&& (membero xs a)
        ])
      )

    let nullo q : goal = (q === nil())

    let caro xs h  : goal = call_fresh (fun tl -> xs === (h % tl))
    let cdro xs tl : goal = call_fresh (fun h  -> xs === (h % tl))
    let hdo = caro
    let tlo = cdro

  end

let (%)  = List.conso
let (%<) = List.(%<)
let (!<) = List.(!<)
let nil  = List.nil

let rec list f = function
| []    -> nil ()
| x::xs -> List.conso (f x) (list f xs)

let rec nat_list = function
| []    -> nil ()
| x::xs -> nat x % nat_list xs

let some = Option.some
let none = Option.none
let pair = Pair.pair
