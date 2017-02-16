(* Testing fancy types for non-rec 'a option type
 * The type ['a option] was chosen because it is simpliest useful polymorphic type.
 * Type ('a,'b) result is here too.
 *)
open MiniKanren
open Tester
open Printf
open ManualReifiers

module Option = struct
  module X = struct
    type 'a t = 'a option
    let fmap f o =
      (* let () = printf "inside fmap of '%s'\n%!" (generic_show o) in *)
      match o with
      | Some x -> Some(f x)
      | None -> None
  end
  include X
  include Fmap1(X)

  let some : ('a,'b) fancy -> ('a option, 'b option logic) fancy = fun x -> inj (distrib (Some x))
  let none : unit -> (_,_ option logic) fancy = fun () -> inj @@ (distrib None )
end

let show_int = GT.(show int)
let show_int_opt = GT.(show option) show_int
let show_intl n = show_logic string_of_int n
let show_intl_optl o = show_logic GT.(show option show_intl) o

let int_opt_reifier = Option.reifier int_reifier

let _ =
  let open Option in
  run_exn show_int 1 q qh (REPR(fun q -> q === inj@@lift 5 ));
  runR int_opt_reifier show_int_opt show_intl_optl 1 q qh (REPR(fun q -> q === some @@ inj_int 5 ));
  runR int_opt_reifier show_int_opt show_intl_optl 1 q qh (REPR(fun q -> q === none() ));
  runR int_reifier     show_int     show_intl      1 q qh (REPR(fun q -> some q === some @@ inj_int 5 ));
  runR int_opt_reifier show_int_opt show_intl_optl 1 q qh (REPR(fun q -> call_fresh (fun w -> (q === some w) )));
  ()

(* about result *)
module Result = struct
  module X = struct
    @type ('a,'b) t = OK of 'a | Error of 'b with show;;

    let fmap f g = function
    | OK x -> OK (f x)
    | Error y -> Error (g y)
  end
  include X
  include Fmap2(X)

  let ok = fun x -> inj @@ distrib @@ OK x
  let error = fun x -> inj @@ distrib @@ Error x
end

let show1 = GT.(show Result.t (show int) (show option @@ show int))
let show1logic =
  show_logic GT.(show Result.t (show_logic (show int)) (show_logic (show option @@ show_logic (show int))))

let runResult n = runR (Result.reifier int_reifier int_opt_reifier) show1 show1logic n

let _ =
  run_exn show1 1  q qh (REPR(fun q -> q === Result.ok @@ inj_int 5 ));
  runResult   (-1) q qh (REPR(fun q -> call_fresh (fun r -> (q === Result.ok r) &&& conde [r===inj_int 5; success]) ));
  runResult   (-1) q qh (REPR(fun q -> Fresh.two (fun r s -> conde
                                                                [ (q === Result.ok s) &&& (s =/= inj_int 4)
                                                                ; (q === Result.error r)
                                                                ]) ));
  ()
