functor TrivialGrader (val description : string) :> GRADER =
  struct
    structure Rubric =
      struct
        val description = description
        type t = unit
        val toString = fn () => "Grading successful."
        val score = fn () => Rational.one
      end

    val process = fn () => ()
  end
