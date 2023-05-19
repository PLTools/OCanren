let () = print_endline "test015"

(* type 'a aaa = AAA of 'a [@@deriving gt ~options:{ gmap }]
type 'b bbb = BBB of 'b [@@deriving gt ~options:{ gmap }] *)

type 'a aaa = 'a GT.list
and 'a bbb = 'a GT.option [@@deriving gt ~options:{ show; gmap }]

let __ (type a b) : (a -> b) -> a aaa -> b aaa = fun eta -> GT.gmap aaa eta
let __ (type a b) : (a -> b) -> a bbb -> b bbb = fun eta -> GT.gmap bbb eta
