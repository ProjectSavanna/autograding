functor EquivGraderBucket (
  val description : string

  type input
  structure Output : OUTPUT

  val tests : (string * input) list
  val timeout : Time.time

  structure Bucket : OUTPUT
  val bucket : input -> Bucket.t
  val buckets : (Bucket.t * int) list

  val refsol     : input -> Output.t
  val submission : input -> Output.t
) :> GRADER =
  BucketGrader (
    val description = description

    type input = input
    structure Output = Output

    val tests = List.map (fn (inputString,input) =>
      let
        val refsolOutput = Result.valOf (Result.evaluate timeout refsol input)
      in
        ((input,fn () => inputString),(
          Fn.curry Output.equal refsolOutput,
          fn () => Output.toString refsolOutput
        ))
      end
    ) tests
    val timeout = timeout

    structure Bucket = Bucket
    val bucket = bucket
    val buckets = buckets

    val submission = submission
  )
