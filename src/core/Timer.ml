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

type t = unit -> Mtime.span
  
let make () =
  let origin = Mtime_clock.elapsed () in
  fun () ->
  Mtime.Span.abs_diff origin (Mtime_clock.elapsed ())

  (*
  let open Unix in
  let origin = (times ()).tms_utime in
  (fun () ->
    (times ()).tms_utime -. origin
  ) *)
    
