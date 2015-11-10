open MiniKanren

let run printer n runner goal =
  run (fun st -> 
    let (repr, result), vars = runner goal st in
    Printf.printf "%s, %s answer%s {\n" 
      repr 
      (if n = (-1) then "all" else string_of_int n) 
      (if n <>  1  then "s" else "");
    List.iter
      (fun st ->        
         List.iter
           (fun (s, x) -> Printf.printf "%s=%s; " s (printer (refine st x)))
           vars;
         Printf.printf "\n"
      )
      (take ~n:n result);
    Printf.printf "}\n%!"
  )
  
