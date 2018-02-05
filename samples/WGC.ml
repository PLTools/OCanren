open GT
open MiniKanren
open Std

@type move = Empty | Goat | Wolf | Cabbage with show
  
let (!) x = inj @@ lift x

let qua x y z t = pair (pair x y) (pair z t)
                       
let [[isGoat; isWolf; isCabbage; isMan] as are; [noGoat; noWolf; noCabbage; noMan] as no] =
  List.map (fun f -> List.map (fun p s -> p f s) [(fun f s -> fresh (x y z) (s === qua !f x y z));
                                                  (fun f s -> fresh (x y z) (s === qua x !f y z));
                                                  (fun f s -> fresh (x y z) (s === qua x y !f z));
                                                  (fun f s -> fresh (x y z) (s === qua x y z !f))]
           ) [true; false]

let safe state = fresh (left right) (
                   (state === pair left right) &&&
                   (let safe' side =
                      isMan side |||
                      (noMan side &&&
                         (noGoat side ||| (isGoat side &&& ((noCabbage side &&& noWolf side) |||
                                                            (isCabbage side &&& isWolf side))
                                          )
                         )
                      )
                    in
                    safe' left &&& safe' right
                   )    
                 )

let swap state state' =
  fresh (left right) (
    (state === pair left right) &&& (state' === pair right left)
  )
        
let step state move state' =
  fresh (left right) (
    (state === pair left right) &&&
    let step' left right state' =
      fresh (lm lg lw lc rm rg rw rc) (
        (left === qua lg lw lc lm) &&& (right === qua rg rw rc rm) &&&
        conde [
          (move === !Empty  )                    &&& (state' === pair (qua lg lw lc !false    ) (qua rg rw rc !true))    &&& safe state';
          (move === !Goat   ) &&& isGoat    left &&& (state' === pair (qua !false lw lc !false) (qua !true rw rc !true)) &&& safe state';
          (move === !Wolf   ) &&& isWolf    left &&& (state' === pair (qua lg !false lc !false) (qua rg !true rc !true)) &&& safe state';
          (move === !Cabbage) &&& isCabbage left &&& (state' === pair (qua lg lw !false !false) (qua rg rw !true !true)) &&& safe state';
        ]
      )
    in    
    conde [
      isMan left  &&& noMan right &&& step' left right state';
      isMan right &&& noMan left  &&& (fresh (state'') (step' right left state'' &&& swap state'' state'))
    ]      
  )

let rec eval state moves state' =
  conde [
    (moves === nil ()) &&& (state === state');
    fresh (move moves' state'') (
      (moves === move % moves') &&&
      (step state move state'') &&&
      (eval state'' moves' state')  
    )
    ]


let reify_state =
  let reify_qua = Pair.reify (Pair.reify reify reify) (Pair.reify reify reify) in
  Pair.reify reify_qua reify_qua

let show_state =
  let show_qua = show(Pair.logic) (show(Pair.logic) (show(Bool.logic)) (show(Bool.logic))) (show(Pair.logic) (show(Bool.logic)) (show(Bool.logic))) in
  show(Pair.logic) show_qua show_qua

let reify_solution s = s#reify @@ List.reify reify
let show_solution = show(List.logic) (show(logic) (show move))

let id x = x
             
let _ =
  Stream.iter (fun s -> Printf.printf "%s\n" @@ show_state @@ s#reify reify_state) @@
  run q (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (nil ()) q) id;
  
  Stream.iter (fun s -> Printf.printf "%s\n" @@ show_state @@ s#reify reify_state) @@
  run q (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (!< !Empty) q) id;

  Stream.iter (fun s -> Printf.printf "%s\n" @@ show_state @@ s#reify reify_state) @@
  run q (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (!< !Goat) q) id; 

  Stream.iter (fun s -> Printf.printf "%s\n" @@ show_state @@ s#reify reify_state) @@
  run q (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) (!< !Wolf) q) id;

  List.iter (fun s -> Printf.printf "%s\n" @@ show_solution @@ reify_solution s) @@ Stream.take ~n:100 @@
  run q (fun q -> eval (pair (qua !true !true !true !true) (qua !false !false !false !false)) q (pair (qua !false !false !false !false) (qua !true !true !true !true))) id;

  
                   
                   
