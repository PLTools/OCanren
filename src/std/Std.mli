(*
 * OCanren.
 * Copyright (C) 2015-2017
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(** {2 Standard relational library} *)

open Logic
open Core

(** Equality as boolean relation *)
val eqo : ('a, 'b logic) injected -> ('a, 'b logic) injected -> LBool.groundi -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b logic) injected -> ('a, 'b logic) injected -> LBool.groundi -> goal

(** [inj_nat n] is a deforested synonym for injection *)
val nat : int -> LNat.groundi

(** [inj_list inj_a l] is a deforested synonym for injection *)
val list : ('a -> ('a, 'b) injected) -> 'a GT.list -> ('a, 'b) LList.groundi

(** [inj_nat_list l] is a deforsted synonym for injection *)
val nat_list : int GT.list -> (LNat.ground, LNat.logic) LList.groundi

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) injected -> ('a,'b) LList.groundi -> ('a,'b) LList.groundi

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) injected -> ('a, 'b) injected -> ('a, 'b) LList.groundi

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) injected -> ('a, 'b) LList.groundi

(** [nil] is a synonym for [inj Nil] *)
val nil : unit -> (_, _) LList.groundi

(** Synonyms for [option] constructors *)
val some : ('a, 'b) injected -> ('a, 'b) LOption.groundi
val none : unit -> ('a, 'b) LOption.groundi

(** Synonym for pair *)
val pair : ('a, 'b) injected -> ('c, 'd) injected -> ('a, 'b, 'c, 'd) LPair.groundi
