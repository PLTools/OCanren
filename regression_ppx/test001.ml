
type ('a, 'b) list = Nil | Cons of 'a * ('a,'b) list [@@distrib] [@@deriving gt ~options:{show; gmap}]
