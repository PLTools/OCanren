ocanren type state    = S ;;

module _ = struct
  ocanren type u = U of state
end

module Move = struct
  ocanren type 'a ground =
    | Forward  of 'a
    | Backward  of 'a
end

ocanren type hum_moves = GT.int Move.ground
