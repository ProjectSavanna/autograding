structure OrderOutput :> OUTPUT where type t = order =
  struct
    type t = order
    val toString = fn
      LESS    => "LESS"
    | EQUAL   => "EQUAL"
    | GREATER => "GREATER"
    val equal : t * t -> bool = op =
  end
