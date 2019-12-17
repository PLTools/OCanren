open OCanren
open OCanren.Std

open GT

module Fmap1 = Fmap


module Fuck = struct
  type term =
    | V of string
    | App of term * term
    | Abs of string * term
             [@@distrib]  [@@deriving gt ~options:{show; gmap}]

  end
