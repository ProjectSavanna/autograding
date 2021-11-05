functor CurProdGrader2 (
  val description : string
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  (* val descriptions : string * string *)
  (* ^ maybe include with weights? *)
  val weights : int * int
) :> GRADER = NewProdGrader2(
  val description = description (* remove? *)
  structure Grader1 = Grader1
  structure Grader2 = Grader2

  local
    val (w1, w2) = weights
    structure Combiner = CurProdCombiner (
      val descriptions = [
        Grader1.Rubric.description,
        Grader2.Rubric.description
      ]
      val weights = [w1, w2]
    )
  in
    open Combiner
  end
)
