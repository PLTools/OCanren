(** {3 Relational numbers} *)

open Logic
open Core

(** Abstract nat type *)
@type 'a nat =
| O
| S of 'a with show, html, eq, compare, foldl, foldr, gmap

(** Type synonym to prevent toplevel [logic] from being hidden *)
type 'a logic' = 'a logic

(** Synonym for abstract nat type *)
type 'a t = 'a nat

(** Ground nat are ismorphic for regular one *)
type ground = ground t

(** Logic nat *)
type logic = logic t logic'

(** GT-compatible typeinfo for [ground] *)
val ground :
  (unit,
   < compare : ground -> ground -> GT.comparison;
     eq      : ground -> ground -> bool;
     foldl   : 'a -> ground -> 'a;
     foldr   : 'a -> ground -> 'a;
     gmap    : ground -> ground;
     html    : ground -> HTML.viewer;
     show    : ground -> string >)
  GT.t

(** GT-compatible typeinfo for [logic] *)
val logic :
  (unit,
   < compare : logic -> logic -> GT.comparison;
     eq      : logic -> logic -> bool;
     foldl   : 'a -> logic -> 'a;
     foldr   : 'a -> logic -> 'a;
     gmap    : logic -> logic;
     html    : logic -> HTML.viewer;
     show    : logic -> string >)
  GT.t

(** Logic injection (for reification) *)
val inj : ground -> logic

(** A type synonym for injected nat *)
type groundi = (ground, logic) injected

(** Reifier *)
val reify : Env.t -> groundi -> logic

(** [of_int n] converts integer [n] into [ground]; negative integers become [O] *)
val of_int : int -> ground

(** [to_int g] converts ground [g] into integer *)
val to_int : ground -> int

(** Make injected [nat] from ground one *)
val nat : ground -> groundi

val o : groundi
val s : groundi -> groundi

val zero : groundi
val one  : groundi
val succ : groundi -> groundi

(** Relational addition *)
val addo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [addo] *)
val ( + ) : groundi -> groundi -> groundi -> goal

(** Relational multiplication *)
val mulo  : groundi -> groundi -> groundi -> goal

(** Infix syninym for [mulo] *)
val ( * ) : groundi -> groundi -> groundi -> goal

(** Comparisons *)
val leo : groundi -> groundi -> LBool.groundi -> goal
val geo : groundi -> groundi -> LBool.groundi -> goal
val gto : groundi -> groundi -> LBool.groundi -> goal
val lto : groundi -> groundi -> LBool.groundi -> goal

(** Comparisons as goals *)
val (<=) : groundi -> groundi -> goal
val (>=) : groundi -> groundi -> goal
val (>)  : groundi -> groundi -> goal
val (<)  : groundi -> groundi -> goal