functor ConditionalPreamble (
  structure Grader : GRADER
  val show : Grader.Rubric.t -> string option
) =
  struct
    open Grader

    structure Rubric =
      struct
        open Rubric
        val separator = String.implode (List.tabulate (80, fn _ => #"="))
        val toString = fn rubric => (
          case show rubric of
            NONE          => ""
          | SOME preamble => preamble ^ "\n" ^ separator ^ "\n"
          ) ^ toString rubric
      end
  end
