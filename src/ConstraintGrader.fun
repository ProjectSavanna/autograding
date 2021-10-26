functor ConstraintGrader (structure Constraint : GRADER
                          structure Standard : GRADER
                          val threshold : Rational.t
                          val message : string) :> GRADER =
  struct
    structure Rubric =
      struct
        val description = "ConstraintGrader: " ^ Constraint.Rubric.description
                            ^ " and " ^ Standard.Rubric.description

        datatype t = Violation
                   | Result of Standard.Rubric.t

        val toString = fn Violation => message
                        | Result r  => Standard.Rubric.toString r

        val score = fn Violation => Rational.zero
                     | Result r  => Standard.Rubric.score r
      end

    val process = fn () =>
      let
        val score = Constraint.Rubric.score (Constraint.process ())
      in
        case Rational.compare (threshold, score) of
          GREATER => Rubric.Violation
        | _       => Rubric.Result (Standard.process ())
      end
  end
