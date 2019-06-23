open Logic
open Core

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

let reify = Logic.reify

let falso = Logic.inj @@ lift false
let truo  = Logic.inj @@ lift true

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