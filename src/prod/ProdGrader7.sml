functor ProdGrader7 (
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  structure Grader3 : GRADER
  structure Grader4 : GRADER
  structure Grader5 : GRADER
  structure Grader6 : GRADER
  structure Grader7 : GRADER
  val weights : int * int * int * int * int * int * int
) :> GRADER =
  struct
    structure Rubric =
      struct
        type t = {
          g1 : Grader1.Rubric.t,
          g2 : Grader2.Rubric.t,
          g3 : Grader3.Rubric.t,
          g4 : Grader4.Rubric.t,
          g5 : Grader5.Rubric.t,
          g6 : Grader6.Rubric.t,
          g7 : Grader7.Rubric.t
        }

        val scores = fn {g1=g1,g2=g2,g3=g3,g4=g4,g5=g5,g6=g6,g7=g7} => [
          Grader1.Rubric.score g1,
          Grader2.Rubric.score g2,
          Grader3.Rubric.score g3,
          Grader4.Rubric.score g4,
          Grader5.Rubric.score g5,
          Grader6.Rubric.score g6,
          Grader7.Rubric.score g7
        ]

        local
          val (w1,w2,w3,w4,w5,w6,w7) = weights
          val weights = [w1,w2,w3,w4,w5,w6,w7]
          val total = List.foldr Int.+ 0 weights
        in
          val fractions = List.map (
            case total of
              0 => Fn.const Rational.one
            | _ => Fn.curry (Fn.flip Rational.//) total
          ) weights
        end

        val toString = fn rubric as {g1=g1,g2=g2,g3=g3,g4=g4,g5=g5,g6=g6,g7=g7} =>
          String.concat (
            ListPair.map (op ^) (
              ListPair.map FormatUtil.showPercents (
                scores rubric,
                fractions
              ),
              List.map FormatUtil.indent [
                Grader1.Rubric.toString g1,
                Grader2.Rubric.toString g2,
                Grader3.Rubric.toString g3,
                Grader4.Rubric.toString g4,
                Grader5.Rubric.toString g5,
                Grader6.Rubric.toString g6,
                Grader7.Rubric.toString g7
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
      g3 = Grader3.process (),
      g4 = Grader4.process (),
      g5 = Grader5.process (),
      g6 = Grader6.process (),
      g7 = Grader7.process ()
    }
  end
