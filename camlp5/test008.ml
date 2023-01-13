
(* open OCanren *)

ocanren type state    = S of GT.int;;
(* type solution = move logic Std.List.logic [@@deriving gt ~options:{show}] *)
ocanren type solution = state OCanren.Std.List.ground

module _ = struct 
  ocanren type u = U of state
end 
