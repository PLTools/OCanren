open OCanren



module T : Timer.T = struct
  type t = Mtime.span

  let elapsed = Mtime_clock.elapsed
  let abs_diff start fin =
    let span = Mtime.Span.abs_diff start fin in
    Mtime.Span.(Timer.{s = to_s span; ms = to_ms span})

end

let () = OCanren.Timer.install_timer (module T)
