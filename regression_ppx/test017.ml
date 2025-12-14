open GT
open OCanren
open OCanren.Std

[%%ocanren_inject
type lam =
  | Var of GT.string
      (** asdfasdfasd adfasdddddddddddddddddddddddddddd asdfasddfasdfasdfasdfasdfasdqer asdf adf  *)
  | Abs of GT.string * lam
      (** asdfasdfasd adfasdddddddddddddddddddddddddddd asdfasddfasdfasdfasdfasdfasd  *)
  | App of lam * lam
[@@deriving gt ~options:{ gmap; show }]]
