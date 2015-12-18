open MiniKanren

let empty_reifier _ _ = ""

let run printer reifier n runner goal =
  run (fun st -> 
    let (repr, result), vars = runner goal st in
    Printf.printf "%s, %s answer%s {\n" 
      repr 
      (if n = (-1) then "all" else string_of_int n) 
      (if n <>  1  then "s" else "");
    List.iter
      (fun st ->        
         List.iter
           (fun (s, x) -> 
	      let v, dc = refine st x in          
              let pv = printer v in  
              match reifier dc v with  
              | "" -> Printf.printf "%s=%s; " s pv
              | r  -> Printf.printf "%s=%s (%s);" s pv r              
	   )
           vars;
         Printf.printf "\n"
      )
      (take ~n:n result);
    Printf.printf "}\n%!"
  )
  
