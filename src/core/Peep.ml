(*
 * OCanren.
 * Copyright (C) 2015-2020
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

let unification_counter      = Core.unification_counter
let unification_time         = Core.unification_time
let walk_counter             = VarSubst.walk_counter
let conj_counter             = Core.conj_counter
let disj_counter             = Core.disj_counter
let delay_counter            = Core.delay_counter
let unwrap_suspended_counter = RStream.unwrap_suspended_counter
let force_counter            = RStream.force_counter
let from_fun_counter         = RStream.from_fun_counter
let bind_counter             = RStream.bind_counter
let mplus_counter            = RStream.mplus_counter
