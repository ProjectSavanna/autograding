structure SymbolOutput :> OUTPUT where type t = string =
  struct
    type t = string
    val toString = Fn.id
    val equal : t * t -> bool = op =
  end
