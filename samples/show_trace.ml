open OCanren


let () =
  if Array.length Sys.argv > 2 then begin
    Printf.eprintf "Usage: %s [filename]\n" @@ Sys.argv.(0) ;
    exit (-1)
  end

let filename =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else begin
    print_string "Filename: " ;
    flush stdout ;
    read_line ()
  end

let () = Format.printf "%a\n" Trace.pp @@ Trace.unmarshal_from_file filename
