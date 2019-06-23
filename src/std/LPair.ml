open Logic
open Core

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

let pair x y = Logic.inj @@ distrib (x, y)
