(** This package after its loading adds timer object to all OCanren modules.
    It's split to separate package to avoid hard dependency on [Mtime] package. *)

open OCanren.Timer

module Impl : T = struct
  type t = Mtime.span

  let elapsed = Mtime_clock.elapsed
  let abs_diff start fin =
    let span = Mtime.Span.abs_diff start fin in
    Mtime.Span.({s = to_s span; ms = to_ms span})

end

let () = OCanren.Timer.install_timer (module Impl)
