structure FormatUtil =
  struct
    local
      val padded = StringCvt.padLeft #" " 4 o Rational.percent
    in
      val showPercents = fn (score,weight) =>
        "(" ^ padded (Rational.* (weight,score)) ^
        "/" ^ padded weight ^ "):"
    end

    local
      val format = fn
        nil => "\n"
      | x :: nil => " " ^ x ^ "\n"
      | l => "\n" ^ (String.concat o List.map (fn s => "  " ^ s ^ "\n")) l
    in
      val indent = format o String.tokens (Fn.curry (op =) #"\n")
    end
  end
