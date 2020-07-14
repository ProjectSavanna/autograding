structure StringOutput :> OUTPUT where type t = string =
  struct
    type t = string
    val toString = fn s => "\"" ^ s ^ "\""
    val equal : t * t -> bool = op =
  end
