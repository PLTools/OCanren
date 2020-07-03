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

(* A type for a runtime configuration *)

type t = {
  mutable do_occurs_check : bool  
}

let data = {do_occurs_check = true}

let do_occurs_check  () = data.do_occurs_check
                       
let occurs_check_on  () = data.do_occurs_check <- true
let occurs_check_off () = data.do_occurs_check <- false
                                                   
