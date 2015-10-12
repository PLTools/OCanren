type t = A of int | B of string | C of t

(*
let unify x y z = ignore ([x; y]); z


let create () =
  List.hd (! (ref[]))

let x = create ()
let y = create ()
let z = create ()

let test x y z _ =
  unify x (A z) (unify z x ())

let test1 = test x y z ()

*)

let x = Obj.magic ()

