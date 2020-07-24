functor ProdGrader5 (
  val description : string
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  structure Grader3 : GRADER
  structure Grader4 : GRADER
  structure Grader5 : GRADER
  val weights : int * int * int * int * int
) :> GRADER =
  struct
    structure Rubric =
      struct
        val description = description

        type t = {
          g1 : Grader1.Rubric.t,
          g2 : Grader2.Rubric.t,
          g3 : Grader3.Rubric.t,
          g4 : Grader4.Rubric.t,
          g5 : Grader5.Rubric.t
        }

        val scores = fn {g1=g1,g2=g2,g3=g3,g4=g4,g5=g5} => [
          Grader1.Rubric.score g1,
          Grader2.Rubric.score g2,
          Grader3.Rubric.score g3,
          Grader4.Rubric.score g4,
          Grader5.Rubric.score g5
        ]

        local
          val (w1,w2,w3,w4,w5) = weights
          val weights = [w1,w2,w3,w4,w5]
          val total = List.foldr Int.+ 0 weights
        in
          val fractions = List.map (
            case total of
              0 => Fn.const Rational.one
            | _ => Fn.curry (Fn.flip Rational.//) total
          ) weights
        end

        local
          val descriptions = [
            Grader1.Rubric.description,
            Grader2.Rubric.description,
            Grader3.Rubric.description,
            Grader4.Rubric.description,
            Grader5.Rubric.description
          ]
          val combine = fn (percent,description) => percent ^ " " ^ description
          val format = fn (description,output) => description ^ "\n" ^ FormatUtil.indent output
        in
          val toString = fn rubric as {g1=g1,g2=g2,g3=g3,g4=g4,g5=g5} =>
            String.concat (
              ListPair.map format (
                ListPair.map combine (
                  ListPair.map FormatUtil.showPercents (
                    scores rubric,
                    fractions
                  ),
                  descriptions
                ),
                [
                  Grader1.Rubric.toString g1,
                  Grader2.Rubric.toString g2,
                  Grader3.Rubric.toString g3,
                  Grader4.Rubric.toString g4,
                  Grader5.Rubric.toString g5
                ]
              )
            )
        end

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
      g5 = Grader5.process ()
    }
  end
