functor EquivGrader (
  val description : string

  type input
  structure Output : OUTPUT

  val tests : (string * input) list
  val timeout : Time.time

  val refsol     : input -> Output.t
  val submission : input -> Output.t
) :> GRADER =
  EquivGraderBucket (
    val description = description

    type input = input
    structure Output = Output

    val tests = tests
    val timeout = timeout

    structure Bucket = UnitOutput
    val bucket = ignore
    val buckets = [((),1)]

    val refsol     = refsol
    val submission = submission
  )
