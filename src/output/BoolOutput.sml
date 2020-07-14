structure BoolOutput :> OUTPUT where type t = bool =
  struct
    type t = bool
    val toString = Bool.toString
    val equal : t * t -> bool = op =
  end
