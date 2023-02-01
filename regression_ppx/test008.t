  $ ../ppx/pp_ocanren_all.exe  test008.ml -pretty | ocamlformat --impl --enable-outside-detected-project --profile=ocamlformat --margin=100 -
  File "test008.ml", line 12, characters 2-87:
  12 |   [%%distrib type 'a xxx = Cons of 'a * 'a xxx [@@deriving gt ~options:{ show; gmap }]]
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Fallthough case with ''a xxx'. Kind = logic. ppx/distrib/ppx_distrib_expander.ml 182
  $ ./test008.exe
  test008
