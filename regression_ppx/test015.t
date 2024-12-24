  $ ./test015diseq.exe
  rel, 1 answer {
  hd1 = _.12
  hd2 = _.14
  tl2 = _.15
  10: { 0: [|  10 =/= _.11 |] }
  
  11: { 0: [|  11 =/= boxed 0 <_.12, _.13> |] }
  
  15: { 0: [|  14 =/= _.12,  15 =/= _.13 |] }
  
   hd2 === 1
  15: { 0: [|  14 =/= _.12,  15 =/= _.13 |] }
  
   tl2 === []
  13: { 0: [|  12 =/= int<1>,  13 =/= int<0> |] }
  
  47
  13: { 0: [|  12 =/= int<1>,  13 =/= int<0> |] }
  
  }
  fun _ ->
    fresh x
      ((Std.list Fun.id [!< x; !< x]) =/=
         (Std.list Fun.id [!< (!! 1); !< (!! 2)])), 1 answer {
  q=_.10;
  }
  fun q ->
    fresh (x y) (trace_index "x" x) (trace_index "y" y) ((x % y) === q)
      ((x % y) =/= (Std.list Fun.id [!! 1; x]))
      (y === (Std.list Fun.id [!! 2])) success, 1 answer {
  x = _.11
  y = _.12
  q=[_.11; 2];
  }
