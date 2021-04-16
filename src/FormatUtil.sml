structure FormatUtil =
  struct
    local
      val padded = StringCvt.padLeft #" " 4 o Rational.percent
    in
      val showPercents = fn (score,weight) =>
        "(" ^ padded (Rational.* (weight,score)) ^
        "/" ^ padded weight ^ "):"
    end

    val indentWith = fn c =>
      String.concat
      o List.mapi (
         fn (0, s) => String.str c ^ " " ^ s ^ "\n"
          | (_, s) =>               "  " ^ s ^ "\n"
        )
      o String.tokens (Fn.curry (op =) #"\n")

    val indent = indentWith #" "
  end
