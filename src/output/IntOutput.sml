structure IntOutput :> OUTPUT where type t = int =
  struct
    type t = int
    val toString = Int.toString
    val equal : t * t -> bool = op =
  end
