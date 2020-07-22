functor ProdGrader2 (
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  val weights : int * int
) :> GRADER =
  struct
    structure Rubric =
      struct
        type t = {
          g1 : Grader1.Rubric.t,
          g2 : Grader2.Rubric.t
        }

        val scores = fn {g1=g1,g2=g2} => [
          Grader1.Rubric.score g1,
          Grader2.Rubric.score g2
        ]

        local
          val (w1,w2) = weights
          val weights = [w1,w2]
          val total = List.foldr Int.+ 0 weights
        in
          val fractions = List.map (
            case total of
              0 => Fn.const Rational.one
            | _ => Fn.curry (Fn.flip Rational.//) total
          ) weights
        end

        val toString = fn rubric as {g1=g1,g2=g2} =>
          String.concat (
            ListPair.map (op ^) (
              ListPair.map FormatUtil.showPercents (
                scores rubric,
                fractions
              ),
              List.map FormatUtil.indent [
                Grader1.Rubric.toString g1,
                Grader2.Rubric.toString g2
              ]
            )
          )

        val score =
          List.foldr Rational.+ Rational.zero
          o Fn.curry (ListPair.map Rational.* ) fractions
          o scores
      end

    val process = fn () => {
      g1 = Grader1.process (),
      g2 = Grader2.process ()
    }
  end
