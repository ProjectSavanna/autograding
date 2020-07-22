functor ProdGrader3 (
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  structure Grader3 : GRADER
  val weights : int * int * int
) :> GRADER =
  struct
    structure Rubric =
      struct
        type t = {
          g1 : Grader1.Rubric.t,
          g2 : Grader2.Rubric.t,
          g3 : Grader3.Rubric.t
        }

        val scores = fn {g1=g1,g2=g2,g3=g3} => [
          Grader1.Rubric.score g1,
          Grader2.Rubric.score g2,
          Grader3.Rubric.score g3
        ]

        local
          val (w1,w2,w3) = weights
          val weights = [w1,w2,w3]
          val total = List.foldr Int.+ 0 weights
        in
          val fractions = List.map (
            case total of
              0 => Fn.const Rational.one
            | _ => Fn.curry (Fn.flip Rational.//) total
          ) weights
        end

        val toString = fn rubric as {g1=g1,g2=g2,g3=g3} =>
          String.concat (
            ListPair.map (op ^) (
              ListPair.map FormatUtil.showPercents (
                scores rubric,
                fractions
              ),
              List.map FormatUtil.indent [
                Grader1.Rubric.toString g1,
                Grader2.Rubric.toString g2,
                Grader3.Rubric.toString g3
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
      g2 = Grader2.process (),
      g3 = Grader3.process ()
    }
  end
