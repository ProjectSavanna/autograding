structure FormatUtil =
  struct
    local
      val padded = StringCvt.padLeft #" " 4 o Rational.percent
    in
      val showPercents = fn (score,weight) =>
        "(" ^ padded (Rational.* (weight,score)) ^
        "/" ^ padded weight ^ "):"
    end

    val indentWith = fn s0 =>
      let
        val spaces = String.implode (List.tabulate (String.size s0, Fn.const #" "))
      in
        String.concat
        o List.mapi (
            fn (i, s) =>
              (case i of
                0 => s0
              | _ => spaces)
              ^ s ^ "\n"
          )
        o String.tokens (Fn.curry (op =) #"\n")
      end

    val indent = indentWith "  "
  end
