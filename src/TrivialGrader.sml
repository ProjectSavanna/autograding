structure TrivialGrader :> GRADER =
  struct
    structure Rubric =
      struct
        type t = unit
        val toString = fn () => "Grading successful."
        val score = fn () => Rational.one
      end

    val process = fn () => ()
  end
