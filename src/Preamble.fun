functor Preamble (
  val preamble : string
  structure Grader : GRADER
) = ConditionalPreamble (
  structure Grader = Grader
  val show : Grader.Rubric.t -> string option = Fn.const (SOME preamble)
)
