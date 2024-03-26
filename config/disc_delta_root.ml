let prj = Sys.argv.(1)
let ws = Sys.argv.(2)

let chop_prefix s ~prefix =
  if String.starts_with ~prefix s then
    StringLabels.sub s ~pos:(String.length prefix)
      ~len:(String.length s - String.length prefix)
  else assert false

let () =
  (* print_endline prj;
     print_endline ws;
     print_endline (Unix.realpath prj);
     print_endline (Unix.realpath ws); *)
  let ans =
    if prj = ws then "."
    else
      let ans = chop_prefix (Unix.realpath prj) ~prefix:(Unix.realpath ws) in
      if String.starts_with ans ~prefix:"/" then chop_prefix ans ~prefix:"/"
      else ans
  in

  (* Format.printf "ans = %s\n%!" ans; *)
  Stdlib.Out_channel.with_open_text "delta-path.cfg" (fun ch ->
      output_string ch ans);
  ()
