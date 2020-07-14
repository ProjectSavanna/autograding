structure UnitOutput :> OUTPUT where type t = unit =
  struct
    type t = unit
    val toString = fn () => "()"
    val equal : t * t -> bool = op =
  end
