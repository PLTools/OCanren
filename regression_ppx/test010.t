  $ ../ppx/pp_ocanren_all.exe  test010.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  File "test010.ml", line 10, characters 2-74:
  10 |   [%%ocanren type nonrec u = U of state [@@deriving gt ~options:{ gmap }]]
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: injectify: non supported case `state`

  $ ./test010.exe
  test010
