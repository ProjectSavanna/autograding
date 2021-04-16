signature EQUIV_INPUT =
  sig
    val description : string

    type input
    structure Output : OUTPUT

    val tests : (string * input) list
    val timeout : Time.time

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end

functor EquivAux (I : EQUIV_INPUT) =
  struct
    open I

    structure Bucket = UnitOutput
    val bucket = ignore
    val buckets = [((),1)]
  end

functor EquivGrader (I : EQUIV_INPUT) :> GRADER =
  EquivGraderBucket (EquivAux (I))

functor EquivGraderList (I :
    sig
      include EQUIV_INPUT
      val cutoff : int
    end
) :> GRADER =
  EquivGraderBucketList (
    struct
      structure S = EquivAux (I)
      open S
      val cutoff = I.cutoff
    end
  )
