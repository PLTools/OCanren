open MiniKanren

let succ prev f = call_fresh (fun x -> prev (f x))

let zero  f = f 
let one   f = succ zero f
let two   f = succ one f
let three f = succ two f
let four  f = succ three f

let q    = one
let qp   = two
let qpr  = three
let qprt = four

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
  
