open MiniKanren
open Tester
open Printf
open GT

module Fmap1 = Fmap


module Fuck = struct
  type term =
    | V of string
    | App of term * term
    | Abs of string * term
             [@@distrib]

  end

(* module Fuck = struct
 *   type ('reg, 'loc, 'value, 'mo, 'op, 't) t =
 *           | Const    of 'value
 *           | Var      of 'reg
 *           | Binop    of 'op * 't * 't
 *           | Asgn     of 't * 't
 *           | Pair     of 't * 't
 *           | If       of 't * 't * 't
 *           | Repeat   of 't
 *           | While    of 't * 't
 *           | Read     of 'mo * 'loc
 *           | Write    of 'mo * 'loc * 't
 *           | Cas      of 'mo * 'mo * 'loc * 't * 't
 *           | Seq      of 't * 't
 *           | Spw      of 't * 't
 *           | Par      of 't * 't
 *           | Skip
 *           | Stuck
 *             [@@deriving distrib]
 * 
 *   end *)
