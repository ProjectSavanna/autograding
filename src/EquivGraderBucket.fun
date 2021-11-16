signature EQUIV_BUCKET_INPUT =
  sig
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
  end

functor EquivAuxBucket (I : EQUIV_BUCKET_INPUT) =
  struct
    open I

    val tests = List.map (fn (inputString,input) =>
      let
        val refsolOutput = Result.valOf (Result.evaluate timeout refsol input)
      in
        ((input, fn () => inputString), (
          Fn.curry Output.equal refsolOutput,
          fn () => Output.toString refsolOutput
        ))
      end
    ) tests
  end

functor EquivGraderBucket (I : EQUIV_BUCKET_INPUT) :> GRADER =
  BucketGrader (EquivAuxBucket (I))

functor EquivGraderBucketList (
  I :
    sig
      include EQUIV_BUCKET_INPUT
      val cutoff : int
    end
) :> GRADER =
  BucketGraderList (
    struct
      structure S = EquivAuxBucket (I)
      open S
      val cutoff = I.cutoff
    end
  )
