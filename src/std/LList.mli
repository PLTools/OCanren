(** {3 Relational Lists} *)

open Logic
open Core

(** Abstract list type *)
@type ('a, 'l) list =
| Nil
| Cons of 'a * 'l with show, gmap, html, eq, compare, foldl, foldr

(** Type synonym to prevent toplevel [logic] from being hidden *)
type 'a logic' = 'a logic

(** Synonym for abstract list type *)
type ('a, 'l) t = ('a, 'l) list

(** Ground lists (isomorphic to regular ones) *)
type 'a ground = ('a, 'a ground) t

(** Logic lists (with the tails as logic lists) *)
type 'a logic  = ('a, 'a logic) t logic'

(** GT-compatible typeinfo for ['a ground] *)
val ground :
  (unit,
   < gmap    : ('a -> 'b) -> 'a ground -> 'b ground;
     compare : ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground -> GT.comparison;
     eq      : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
     foldl   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
     foldr   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
     html    : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer;
     show    : ('a -> string) -> 'a ground -> string >)
  GT.t

(** GT-compatible typeinfo for ['a logic] *)
val logic :
  (unit,
    < gmap    : ('a -> 'b) -> (('a, 'c) t logic' as 'c) -> (('b, 'd) t logic' as 'd);
      compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
      eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
      foldr   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
      foldl   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
      html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
      show    : ('a -> string) -> 'a logic -> GT.string  >)
    GT.t

(** A synonym for injected list *)
type ('a,'b) groundi = ('a ground, 'b logic) injected

(** Constructors *)
val nil : unit -> ('a, 'b) groundi

val cons : ('a, 'b) injected -> ('a, 'b) groundi -> ('a, 'b) groundi

(** [of_list l] converts regular OCaml list [l] into isomorphic OCanren [ground] list *)
val of_list : ('a -> 'b) -> 'a GT.list -> 'b ground

(** [to_list g] converts OCanren list [g] into regular OCaml list *)
val to_list : ('a -> 'b) -> 'a ground -> 'b GT.list

(** [inj x] makes a logic list from a ground one *)
val inj : ('a -> 'b) -> 'a ground -> 'b logic

(** Make injected [list] from ground one of injected elements *)
val list : ('a, 'b) injected GT.list -> ('a, 'b) groundi

(** Reifier *)
val reify : (Env.t -> ('a, 'b) injected -> 'b) -> Env.t -> ('a ground, 'b logic) injected -> 'b logic

(** Relational foldr *)
val foldro : (('a, 'b) injected -> ('acc, _ logic' as 'acc2) injected -> ('acc, 'acc2) injected -> goal) ->
             ('acc, 'acc2) injected -> ('a, 'b) groundi -> ('acc, 'acc2) injected -> goal

(** Relational map *)
val mapo : (('a, 'b) injected -> ('q, 'w) injected -> goal) -> ('a, 'b) groundi -> ('q, 'w) groundi -> goal

(** Relational filter *)
val filtero : (('a, 'b) injected -> LBool.groundi -> goal) -> ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Relational lookup *)
val lookupo : (('a, 'b) injected -> LBool.groundi -> goal) -> ('a, 'b) groundi -> ('a option, 'b option logic') injected -> goal

(** Boolean list disjunctions *)
val anyo : (LBool.ground, LBool.logic) groundi -> LBool.groundi -> goal

(** Boolean list conjunction *)
val allo : (LBool.ground, LBool.logic) groundi -> LBool.groundi -> goal

(** Relational length *)
val lengtho : (_, _) groundi -> LNat.groundi -> goal

(** Relational append *)
val appendo : ('a, 'b) groundi -> ('a, 'b) groundi  -> ('a, 'b) groundi -> goal

(** Relational reverse *)
val reverso : ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Relational occurrence check (a shortcut) *)
val membero : ('a, 'b logic') groundi  -> ('a, 'b logic') injected  -> goal

(** Relational check for empty list *)
val nullo : _ groundi -> goal

(** Relational head of the list *)
val caro  : ('a, 'b) groundi -> ('a, 'b) injected -> goal

(** Alias for [caro] *)
val hdo   : ('a, 'b) groundi -> ('a, 'b) injected -> goal

(** Relational tail of the list *)
val cdro  : ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Alias for [cdro] *)
val tlo   : ('a, 'b) groundi -> ('a, 'b) groundi -> goal

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) injected -> ('a,'b) groundi -> ('a,'b) groundi

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) groundi

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) injected -> ('a, 'b) groundi