functor ProdGrader2 (
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  val descriptions : string * string
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

        local
          val (d1,d2) = descriptions
          val combine = fn (percent,description) => percent ^ " " ^ description
          val format = fn (description,output) => description ^ "\n" ^ FormatUtil.indent output
        in
          val toString = fn rubric as {g1=g1,g2=g2} =>
            String.concat (
              ListPair.map format (
                ListPair.map combine (
                  ListPair.map FormatUtil.showPercents (
                    scores rubric,
                    fractions
                  ),
                  [d1,d2]
                ),
                [
                  Grader1.Rubric.toString g1,
                  Grader2.Rubric.toString g2
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
      g2 = Grader2.process ()
    }
  end
