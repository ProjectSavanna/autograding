signature OUTPUT =
  sig
    type t
    val toString : t -> string
    val equal : t * t -> bool
  end
