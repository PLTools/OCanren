(** {3 Logic values} *)

(** A type of a logic value *)
@type 'a logic = private
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

(** GT-compatible typeinfo for logics *)
val logic :
  (unit,
   < show    : ('a -> string) -> 'a logic -> string;
     html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
     eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
     compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
     foldl   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     foldr   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     gmap    : ('a -> 'sa) -> 'a logic -> 'sa logic
   >) GT.t

(** [to_logic x] makes a logic value from a regular one *)
val to_logic : 'a -> 'a logic

(** [from_logic x] makes a regular value from a logic one.
    Raises exception [Not_a_value] if [x] contains free variables
*)
val from_logic : 'a logic -> 'a

(** {3 Injections/projections} *)

(**  The type [('a, 'b) injected] describes an injection of a type ['a] into ['b] *)
type ('a, 'b) injected

(** [lift x] lifts [x] into injected doamin *)
val lift : 'a -> ('a, 'a) injected

(** [inj x] injects [x] into logical [x] *)
val inj : ('a, 'b) injected -> ('a, 'b logic) injected

(** A synonym for [fun x -> inj @@ lift x] (for non-parametric types) *)
val (!!) : 'a -> ('a, 'a logic) injected

(** [prj x] returns a regular value from injected representation.
    Raises exception [Not_a_value] if [x] contains free variables
*)
val prj : ('a, 'b) injected -> 'a

(** The exception is raised when we try to extract a regular term from the answer with some free variables *)
exception Not_a_value

(** Reification result *)
class type ['a,'b] reified =
object
  (** Returns [true] if the term has any free logic variable inside *)
  method is_open: bool

  (** Gets the answer as regular term. Raises exception [Not_a_value] when the answer contains free variables *)
  method prj: 'a

  (** Gets the answer as a logic value using provided injection function [inj] *)
  method reify: (Env.t -> ('a, 'b) injected -> 'b) -> 'b

  method prjc : (Env.t -> ('a, 'b) injected -> 'a) -> 'a
end

