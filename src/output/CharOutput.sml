structure CharOutput :> OUTPUT where type t = char =
  struct
    type t = char
    val toString = fn c => "#\"" ^ Char.toString c ^ "\""
    val equal : t * t -> bool = op =
  end
