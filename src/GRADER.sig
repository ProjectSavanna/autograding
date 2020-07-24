signature GRADER =
  sig
    structure Rubric :
      sig
        val description : string
        include SHOW
        val score : t -> Rational.t
      end

    val process : unit -> Rubric.t
  end
