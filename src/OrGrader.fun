functor OrGrader (structure Grader1 : GRADER
                  structure Grader2 : GRADER) :> GRADER =
  struct
    structure Rubric =
      struct
        val description = "ConstraintGrader: " ^ Constraint.Rubric.description
                            ^ " and " ^ Standard.Rubric.description

        datatype t = Result1 of Grader1.Rubric.t
                   | Result2 of Grader2.Rubric.t

        val toString = fn Result1 r => Grader1.Rubric.toString r
                        | Result2 r => Grader2.Rubric.toString r

        val score = fn Result1 r => Grader1.Rubric.score r
                     | Result2 r => Grader2.Rubric.score r
      end

    val process = fn () =>
      let
        val process1 = Grader1.process ()
        val score1   = Grader1.Rubric.score process1
        val process2 = Grader2.process ()
        val score2   = Grader2.Rubric.score process2
      in
        case Rational.compare (score1, score2) of
          LESS => Rubric.Result2 process2
        | _    => Rubric.Result1 process1
      end
  end
