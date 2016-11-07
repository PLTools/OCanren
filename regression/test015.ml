open GT
open MiniKanren

type term =
    | Symb  of string logic
    | Seq   of term   logic List.logic

let rec show_term = (function
    | Symb s -> show logic (fun x -> x) s
    | Seq  s -> 
        "(" ^ String.concat " " (List.map (show logic show_term) (List.to_list @@ List.prj (fun x -> x) s)) ^ ")")

let show_term = show logic show_term



let rec lookupo x env r =
    fresh (y t env')
        (env === !!(y, t) % env')
        (conde [
            (y === x) &&& (r === t);
            (y =/= x) &&& (lookupo x env' r)
        ])

let rec not_in_envo x env =
    conde [
        (env === nil);
        fresh (y t env')
            (env === !!(y, t) % env')
            (y =/= x)
            (not_in_envo x env')
    ];



type result =
    | Closure of string logic * term logic * (string logic * result logic) logic List.logic
    | Val     of term   logic

let rec map_evalo es env rs =
    conde [
        (es === nil) &&& (rs === nil);
        fresh (e es' r rs')
            (es === e % es')
            (rs === r % rs')
            (evalo e env !!(Val r))
            (map_evalo es' env rs')
    ]
and evalo term env r =
    conde [
        fresh (t)
            (term === !!(Seq (!!(Symb !!"quote") %< t)))
            (r === !!(Val t))
            (not_in_envo !!"quote" env);
        fresh (s)
            (term === !!(Symb s))
            (lookupo s env r);
        fresh (x body)
            (term === !!(Seq (!!(Symb !!"lambda")      %
                              (!!(Seq (!< !!(Symb x))) %<
                              body))))
            (r === !!(Closure (x, body, env)))
            (not_in_envo !!"lambda" env);
        fresh (es rs)
            (term === !!(Seq (!!(Symb !!"list") % es)))
            (r === !!(Val !!(Seq rs)))
            (not_in_envo !!"list" env)
            (map_evalo es env rs)(*(List.mapo (fun e r -> evalo e env !!(Val r)) es rs)*);
        fresh (func arge arg x body env')
            (term === !!(Seq (func %< arge)))
            (evalo arge env arg)
            (evalo func env !!(Closure (x, body, env')))
            (evalo body (!!(x, arg) % env') r);
    ]

let ( ~~ ) s  = !!(Symb !!s)
let s      tl = !!(Seq (List.inj (fun x -> x) @@ List.of_list tl))

let quineo q =
    fresh (x y)
        (evalo q nil !!(Val q))

let twineso q p =
    (q =/= p) &&& (evalo q nil !!(Val p)) &&& (evalo p nil !!(Val q))



let run_term t = Printf.printf "> %s\n%s\n\n" (show_term t) @@
    run q (fun q -> evalo t nil !!(Val q)) (fun qs -> if Stream.is_empty qs 
                                                then "fail" 
                                                else show_term @@ Stream.hd qs)

let gen_terms n r = Printf.printf "> %s\n" (show_term r);
                    run q (fun q -> evalo q nil !!(Val r)) 
                          (fun qs -> List.iter (fun t -> Printf.printf "%s\n" @@ show_term t) @@ 
                               Stream.take ~n:n qs);
                    Printf.printf "\n"

let find_quines n = run q (fun q -> quineo q) 
                          (fun qs -> List.iter (fun t -> Printf.printf "%s\n\n" @@ show_term t) @@ 
                                Stream.take ~n:n qs)

let find_twines () = run qr (fun q r -> twineso q r) (fun qs rs -> Printf.printf "%s,\n%s\n\n" (show_term @@ Stream.hd qs) (show_term @@ Stream.hd rs))



let quine_c =
    s[s[~~"lambda"; s[~~"x"]; 
        s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]]; 
      s[~~"quote";
        s[~~"lambda"; s[~~"x"];
            s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]]]]

let _ =
    Printf.printf "Evaluate:\n\n%!";
    run_term @@ ~~"x";
    run_term @@ s[s[~~"quote"; ~~"x"]; s[~~"quote"; ~~"y"]];
    run_term @@ s[~~"quote"; ~~"x"; ~~"y"];
    run_term @@ s[~~"quote"; ~~"x"];
    run_term @@ s[~~"list"];
    run_term @@ s[~~"list"; s[~~"quote"; ~~"x"]; s[~~"quote"; ~~"y"]];
    run_term @@ s[s[~~"lambda"; s[~~"x"]; ~~"x"]; s[~~"list"]];
    run_term @@ s[s[s[~~"lambda"; s[~~"x"]; s[~~"lambda"; s[~~"y"]; s[~~"list"; ~~"x"; ~~"y"]]]; s[~~"quote"; ~~"1"]]; s[~~"quote"; ~~"2"]];
    run_term @@ s[s[~~"lambda"; s[~~"lambda"]; s[~~"lambda"; s[~~"list"]]]; s[~~"lambda"; s[~~"x"]; ~~"x"]];
    run_term @@ s[~~"quote"; ~~"list"];
    run_term @@ quine_c;

    Printf.printf "%!Generate:\n\n%!";
    gen_terms 5 @@ ~~"x";
    gen_terms 5 @@ s[];
    gen_terms 5 @@ s[~~"lambda"; s[~~"x"]; s[~~"x"; ~~"y"; ~~"z"]];

    Printf.printf "%!Quines:\n\n%!";
    find_quines 5;

    Printf.printf "%!Twines:\n\n%!";
    find_twines ()
