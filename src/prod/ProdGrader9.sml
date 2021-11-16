functor ProdGrader9 (
  val description : string
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  structure Grader3 : GRADER
  structure Grader4 : GRADER
  structure Grader5 : GRADER
  structure Grader6 : GRADER
  structure Grader7 : GRADER
  structure Grader8 : GRADER
  structure Grader9 : GRADER
  val weights : Rational.Int.int * Rational.Int.int * Rational.Int.int * Rational.Int.int * Rational.Int.int * Rational.Int.int * Rational.Int.int * Rational.Int.int * Rational.Int.int
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
          g5 : Grader5.Rubric.t,
          g6 : Grader6.Rubric.t,
          g7 : Grader7.Rubric.t,
          g8 : Grader8.Rubric.t,
          g9 : Grader9.Rubric.t
        }

        val scores = fn {g1=g1,g2=g2,g3=g3,g4=g4,g5=g5,g6=g6,g7=g7,g8=g8,g9=g9} => [
          Grader1.Rubric.score g1,
          Grader2.Rubric.score g2,
          Grader3.Rubric.score g3,
          Grader4.Rubric.score g4,
          Grader5.Rubric.score g5,
          Grader6.Rubric.score g6,
          Grader7.Rubric.score g7,
          Grader8.Rubric.score g8,
          Grader9.Rubric.score g9
        ]

        val fractions =
          let
            val (w1,w2,w3,w4,w5,w6,w7,w8,w9) = weights
            val weights = [w1,w2,w3,w4,w5,w6,w7,w8,w9]
            val total = List.foldr (op +) 0 weights
          in
            List.map
              (case total of
                0 => Fn.const Rational.one
              | _ => Fn.curry (Fn.flip Rational.//) total)
              weights
          end

        local
          val descriptions = [
            Grader1.Rubric.description,
            Grader2.Rubric.description,
            Grader3.Rubric.description,
            Grader4.Rubric.description,
            Grader5.Rubric.description,
            Grader6.Rubric.description,
            Grader7.Rubric.description,
            Grader8.Rubric.description,
            Grader9.Rubric.description
          ]
          val combine = fn (percent,description) => percent ^ " " ^ description
          val format = fn (description,output) => description ^ "\n" ^ FormatUtil.indent output
        in
          val toString = fn rubric as {g1=g1,g2=g2,g3=g3,g4=g4,g5=g5,g6=g6,g7=g7,g8=g8,g9=g9} =>
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
                  Grader5.Rubric.toString g5,
                  Grader6.Rubric.toString g6,
                  Grader7.Rubric.toString g7,
                  Grader8.Rubric.toString g8,
                  Grader9.Rubric.toString g9
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
      g5 = Grader5.process (),
      g6 = Grader6.process (),
      g7 = Grader7.process (),
      g8 = Grader8.process (),
      g9 = Grader9.process ()
    }
  end
