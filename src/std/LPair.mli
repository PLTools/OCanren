(** {3 Relational pairs} *)

open Logic
open Core

(** Type synonym to prevent toplevel [logic] from being hidden *)
type 'a logic' = 'a logic

(** Synonym for regular option type *)
type ('a, 'b) t = 'a * 'b

(** Ground option (the regular one) *)
type ('a, 'b) ground = 'a * 'b

(** Logic option *)
type ('a, 'b) logic = ('a * 'b) logic'

(** GT-compatible typeinfo for [ground] *)
val ground :
  (unit,
   < compare : ('a -> 'a -> GT.comparison) -> ('b -> 'b -> GT.comparison) -> ('a, 'b) ground -> ('a, 'b) ground -> GT.comparison;
     eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) ground -> ('a, 'b) ground -> bool;
     foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) ground -> 'c;
     foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) ground -> 'c;
     gmap    : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) ground -> ('c, 'd) ground;
     html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) -> ('a, 'b) ground -> HTML.viewer;
     show    : ('a -> string) -> ('b -> string) -> ('a, 'b) ground -> string >)
  GT.t

(** GT-compatible typeinfo for [logic] *)
val logic :
  (unit,
   < compare : ('a -> 'a -> GT.comparison) -> ('b -> 'b -> GT.comparison) -> ('a, 'b) logic -> ('a, 'b) logic -> GT.comparison;
     eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) logic -> ('a, 'b) logic -> bool;
     foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) logic -> 'c;
     foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) logic -> 'c;
     gmap    : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) logic -> ('c, 'd) logic;
     html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) -> ('a, 'b) logic -> HTML.viewer;
     show    : ('a -> string) -> ('b -> string) -> ('a, 'b) logic -> string >)
  GT.t

(** Logic injection (for reification) *)
val inj : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) ground -> ('c, 'd) logic

(** A synonym for injected pair *)
type ('a, 'b, 'c, 'd) groundi = (('a, 'c) ground, ('b, 'd) logic) injected

(** Make injected pair from ground one with injected components *)
val pair : ('a, 'b) injected -> ('c, 'd) injected -> ('a, 'b, 'c, 'd) groundi

(** Reifier *)
val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> Env.t -> ('a, 'b, 'c, 'd) groundi -> ('b, 'd) logic
