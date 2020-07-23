structure FormatUtil =
  struct
    local
      val padded = StringCvt.padLeft #" " 4 o Rational.percent
    in
      val showPercents = fn (score,weight) =>
        "(" ^ padded (Rational.* (weight,score)) ^
        "/" ^ padded weight ^ "):"
    end

    val indent =
      String.concat
      o List.map (fn s => "  " ^ s ^ "\n")
      o String.tokens (Fn.curry (op =) #"\n")
  end
