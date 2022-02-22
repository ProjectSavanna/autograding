functor Preamble (
  val preamble : string
  val show : Rational.t -> bool
  structure Grader : GRADER
) =
  struct
    open Grader

    structure Rubric =
      struct
        open Rubric
        val separator = String.implode (List.tabulate (80, fn _ => #"="))
        val toString = fn rubric => (
          if show (score rubric)
          then preamble ^ "\n" ^ separator ^ "\n"
          else "") ^ toString rubric
      end
  end
