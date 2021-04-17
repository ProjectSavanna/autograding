functor Preamble (
  val preamble : string
  structure Grader : GRADER
) =
  struct
    open Grader

    structure Rubric =
      struct
        open Rubric
        val separator = String.implode (List.tabulate (80, fn _ => #"="))
        val toString = fn rubric =>
          preamble ^ "\n" ^ separator ^ "\n" ^ toString rubric
      end
  end
