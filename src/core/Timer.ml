(*
 * OCanren.
 * Copyright (C) 2015-2022
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

type span = { ms: float; s: float }

let empty_span = { ms=0.0; s=0.0 }

let add_span {ms;s} {ms=ms2; s=s2} = { ms = ms +. ms2; s = s +. s2 }

module type T = sig
  type t

  val elapsed : unit -> t
  val abs_diff : t -> t -> span
end

let timer : (module T) option ref = ref None

let install_timer t = timer := Some t

let make () =
  match !timer with
  | None -> failwith "Timer is not installed. Add linkage with OCanren.install_timer"
  | Some ((module Timer : T) ) ->
    let origin = Timer.elapsed () in
    fun () ->
      let next = Timer.elapsed () in
      Timer.abs_diff next origin
