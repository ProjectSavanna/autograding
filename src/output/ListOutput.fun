functor ListOutput (Output : OUTPUT) :> OUTPUT where type t = Output.t list =
  struct
    type t = Output.t list

    local
      val rec aux = fn
        nil      => "]"
      | x :: nil => Output.toString x ^ "]"
      | x :: xs  => Output.toString x ^ ", " ^ aux xs
    in
      val toString = Fn.curry (op ^) "[" o aux
    end

    val equal = ListPair.allEq Output.equal
  end
