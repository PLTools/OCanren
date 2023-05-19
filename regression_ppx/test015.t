  $ ../ppx/pp_ocanren_all.exe test015.ml -pretty -new-typenames
  let () = print_endline "test015"
  type 'a aaa = 'a GT.list
  and 'a bbb = 'a GT.option[@@deriving gt ~options:{ show; gmap }]
  let __ (type a) (type b) =
    (fun eta -> GT.gmap aaa eta : (a -> b) -> a aaa -> b aaa)
  let __ (type a) (type b) =
    (fun eta -> GT.gmap bbb eta : (a -> b) -> a bbb -> b bbb)
  $ ./test015.exe
  test015
