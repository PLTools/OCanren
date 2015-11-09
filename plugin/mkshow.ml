#load "q_MLast.cmo";;

open Pa_gt.Plugin
open Printf
open List

let _ =
  register "mkshow" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh_t       = <:ctyp< MiniKanren.State.t >>; 
          syn_t       = T.id "string";
          proper_args = d.type_args; 
          fixed_inh   = None;
          sname       = (fun _ -> T.id "string");
          iname       = (fun _ -> <:ctyp< MiniKanren.State.t >>)
        }, 
	let wrap_id l = 
	  map 
	    (fun (x, y) -> 
	      x, y, 
	      (fun env arg x ->
		 E.app [
                   <:expr< MiniKanren.show_var >>;
		   E.lid env.inh;
		   arg;
		   (E.func [<:patt< () >>] x)
	         ]
	      )
	    ) 
	    l 
	in
        let (@@) x y = E.app [E.lid "^"; x; y] in
        let rec body env start stop delim args = 
          (snd 
	     (fold_left 
		(fun (first, expr as acc) arg ->
                  let append e = false, expr @@ (if first then e else (E.str ", ") @@ e) in
                  match arg with                     
		  | arg, (Variable _ | Self _), wrapper -> 
		      append (wrapper env <:expr< $E.lid arg$.GT.x >> <:expr< $E.lid arg$.GT.fx $E.lid env.inh$ >>)
		  | arg, Tuple (_, elems), wrapper ->
		      let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in			
		      append (
                        <:expr<
                           let $P.tuple (map P.id args)$ = $E.lid arg$ in
			   $wrapper env (E.lid arg) (body env (E.str "(") (E.str ")") (E.str ", ") (wrap_id (combine args elems)))$
                        >>
		      )
                  | arg, typ, wrapper -> 
		      (match env.trait "mkshow" typ with
		       | Some e -> append (wrapper env (E.lid arg) <:expr< $e$ $E.lid env.inh$ $E.lid arg$>>)
		       | None   -> acc
		      )
		)         
		(true, start)
                args 
	     )
          ) @@ stop
	in
        object
	  inherit generator
	  method record env fields = 
            body env (E.str "{") (E.str "}") (E.str "; ") (map (fun (a, (f, _, t)) -> a, t, (fun env arg x -> (E.str (f ^ "=")) @@ x)) fields)
	  method tuple env elems = 
	    body env (E.str "(") (E.str ")") (E.str ", ") (wrap_id elems)
	  method constructor env name args = 
	    body env (E.str ((if d.is_polyvar then "`" else "") ^ name ^ " (")) (E.str ")") (E.str ", ") (wrap_id args)
          method default e = 
            let args = List.map (fun a -> "p" ^ a) d.type_args @ ["st"; "x"] in
            let pags = List.map P.id args in
            let eags = List.map E.id args in
            let eapp = E.app (e::eags) in 
            E.func pags <:expr< MiniKanren.show_var st x (fun () -> $eapp$) >>
	end
       )
    )
    
