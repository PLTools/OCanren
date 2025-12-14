module _ = struct

  ocanren type value = Closure of env
  and          env   = value OCanren.Std.List.ground
end

let () = print_endline "test00"
