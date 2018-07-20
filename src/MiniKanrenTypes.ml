module Ts = struct
module type T0 =
sig
  type t
  val fmap :  t -> t
end

module type T1 =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type T2 =
  sig
   type ('a, 'b) t
   val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end

module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
  end

module type T4 =
  sig
    type ('a, 'b, 'c, 'd) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
  end

module type T5 =
  sig
    type ('a, 'b, 'c, 'd, 'e) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
  end

module type T6 =
  sig
    type ('a, 'b, 'c, 'd, 'e, 'f) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

end


module type Fmaps = sig
  open Ts

  type helper
  type ('a, 'b) inj
  type 'a logic_

module Fmap0 (T : T0) :
  sig
    val distrib : T.t -> (T.t, T.t) inj
    val reify : helper -> (T.t, T.t logic_ as 'r) inj -> 'r
    val prjc : (int -> 'r list -> (T.t as 'r)) -> helper -> (T.t, T.t logic_) inj -> 'r
  end

module Fmap : functor (T : T1) ->
  sig
    val distrib : ('a,'b) inj T.t -> ('a T.t, 'b T.t) inj

    val reify : (helper -> ('a,'b) inj -> 'b) -> helper -> ('a T.t, 'b T.t logic_ as 'r) inj -> 'r

    val prjc  : (helper -> ('a,'b) inj -> 'a) ->
      (int -> 'r list -> ('a T.t as 'r)) ->
      helper -> ('r, 'b T.t logic_) inj -> 'r
  end

module Fmap2 (T : T2) :
  sig
    val distrib : (('a,'c) inj, ('b,'d) inj) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) inj

    val reify : (helper -> ('a, 'b) inj -> 'b) -> (helper -> ('c, 'd) inj -> 'd) -> helper -> (('a, 'c) T.t, ('b, 'd) T.t logic_ as 'r) inj -> 'r

    val prjc  : (helper -> ('a, 'b) inj -> 'a) ->
      (helper -> ('c, 'd) inj -> 'c) ->
      (int -> 'r list -> (('a,'c) T.t as 'r) ) ->
      helper -> ('r, ('b,'d) T.t logic_) inj -> 'r
  end

module Fmap3 (T : T3) :
  sig
    val distrib : (('a,'b) inj, ('c, 'd) inj, ('e, 'f) inj) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) inj

    val reify : (helper -> ('a, 'b) inj -> 'b) -> (helper -> ('c, 'd) inj -> 'd) -> (helper -> ('e, 'f) inj -> 'f) ->
                helper -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic_ as 'r) inj -> 'r

    val prjc  : (helper -> ('a, 'b) inj -> 'a) ->
      (helper -> ('c, 'd) inj -> 'c) ->
      (helper -> ('e, 'f) inj -> 'e) ->
      (int -> 'r list -> 'r) ->
      helper -> (('a,'c,'e) T.t as 'r, ('b,'d,'f) T.t logic_) inj -> 'r
  end

module Fmap4 (T : T4) :
  sig
    val distrib : (('a,'b) inj, ('c, 'd) inj, ('e, 'f) inj, ('g, 'h) inj) T.t ->
                       (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) inj

    val reify : (helper -> ('a, 'b) inj -> 'b) -> (helper -> ('c, 'd) inj -> 'd) ->
                (helper -> ('e, 'f) inj -> 'f) -> (helper -> ('g, 'h) inj -> 'h) ->
                helper -> (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t logic_ as 'r) inj -> 'r

    val prjc  :
      (helper -> ('a, 'b) inj -> 'a) -> (helper -> ('c, 'd) inj -> 'c) ->
      (helper -> ('e, 'f) inj -> 'e) -> (helper -> ('g, 'h) inj -> 'g) ->
      (int -> 'r list -> 'r) ->
      helper -> ('r, ('b,'d,'f,'h) T.t logic_) inj -> (('a,'c,'e,'g) T.t as 'r)
  end

module Fmap5 (T : T5) :
  sig
    val distrib : (('a,'b) inj, ('c, 'd) inj, ('e, 'f) inj, ('g, 'h) inj, ('i, 'j) inj) T.t ->
                       (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) inj

    val reify : (helper -> ('a, 'b) inj -> 'b) -> (helper -> ('c, 'd) inj -> 'd) -> (helper -> ('e, 'f) inj -> 'f) ->
                (helper -> ('g, 'h) inj -> 'h) -> (helper -> ('i, 'j) inj -> 'j) ->
                helper -> (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t logic_ as 'r) inj -> 'r

    val prjc  :
      (helper -> ('a, 'b) inj -> 'a) -> (helper -> ('c, 'd) inj -> 'c) ->
      (helper -> ('e, 'f) inj -> 'e) -> (helper -> ('g, 'h) inj -> 'g) ->
      (helper -> ('i, 'j) inj -> 'i) ->
      (int -> 'r list -> 'r) ->
      helper -> ('r, ('b,'d,'f,'h,'j) T.t logic_) inj ->
      (('a,'c,'e,'g,'i) T.t as 'r)
  end

module Fmap6 (T : T6) :
  sig
    val distrib : (('a,'b) inj, ('c, 'd) inj, ('e, 'f) inj, ('g, 'h) inj, ('i, 'j) inj, ('k, 'l) inj) T.t ->
                       (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) inj

    val reify : (helper -> ('a, 'b) inj -> 'b) -> (helper -> ('c, 'd) inj -> 'd) -> (helper -> ('e, 'f) inj -> 'f) ->
                (helper -> ('g, 'h) inj -> 'h) -> (helper -> ('i, 'j) inj -> 'j) -> (helper -> ('k, 'l) inj -> 'l) ->
                helper -> (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t logic_ as 'r) inj -> 'r

    val prjc  :
      (helper -> ('a, 'b) inj -> 'a) -> (helper -> ('c, 'd) inj -> 'c) ->
      (helper -> ('e, 'f) inj -> 'e) -> (helper -> ('g, 'h) inj -> 'g) ->
      (helper -> ('i, 'j) inj -> 'i) -> (helper -> ('k, 'l) inj -> 'k) ->
      (int -> 'r list -> 'r) ->
      helper -> ('r, ('b,'d,'f,'h,'j,'l) T.t logic_) inj ->
      (('a,'c,'e,'g,'i,'k) T.t as 'r)
  end

end
