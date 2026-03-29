module L = List
         
open GT
open OCanren
open OCanren.Std

let addo x y z =
  let open Nat in
  ocanren {
    x == o & y == z |
    fresh x', z' in
      x == S x' & z == S z' & addo x' y z'
  }

let () =
  let counter = Stdlib.ref 1 in
  (ocanrun (q, r : ^Nat.nat) {addo q r 2} -> (Trace.extract_last (), show(Nat.logic) q, show(Nat.logic) r))
    |> Stream.take ~n:(-1)
    |> L.iter begin fun (trace, q, r) ->
      Format.printf "q=%s, r=%s\n" q r ;
      Format.printf "TRACE: %a\n" Trace.pp trace ;
      Trace.marshal_to_file (Format.sprintf "add_%d.trace" !counter) trace ;
      incr counter
    end ;
  Format.printf "Saved %d traces\n" (!counter - 1)

let _ =
  L.iter (fun q -> Format.printf "q=%s\n" q) @@
  Stream.take ~n:(-1) @@
  ocanrun (q : ^Nat.nat) {addo q 1 0} -> (show(Nat.logic) q)
