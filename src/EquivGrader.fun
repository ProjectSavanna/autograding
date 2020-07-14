functor EquivGrader (
  (* input to testing function *)
  type input

  (* output of testing function *)
  structure Output : OUTPUT

  (* submission implementation to grade *)
  val submission : input -> Output.t

  (* reference solution implementation to grade against *)
  val refsol : input -> Output.t

  (* list of tests and corresponding string representations *)
  val tests : (string * input) list

  (* short description of test suite *)
  val description : string

  (* timeout value for each individual test case *)
  val timeout : Time.time
) :> GRADER =
  struct
    val eval = fn f => Result.evaluate timeout f

    structure Rubric =
      struct
        type t = (string * Output.t * Output.t Result.t) option

        local
          val incorrect = fn (input_string,output_refsol,output_submission) =>
            "Test failed:\n" ^
            "  Test    : " ^ input_string ^ "\n" ^
            "  Expected: " ^ Output.toString output_refsol ^ "\n" ^
            "  Received: " ^ Result.toString Output.toString output_submission ^ "\n"
        in
          val toString = fn counterexample =>
            description ^ " " ^ (
              case counterexample of
                NONE => "All tests passed.\n"
              | SOME x => incorrect x
            )
        end

        val score = fn
          NONE   => Rational.one
        | SOME _ => Rational.zero
      end

    infix |>
    fun x |> f = f x

    infix 5 &&&
    fun f &&& g = fn x => (f x, g x)

    infix 5 ***
    fun f *** g = fn (x,y) => (f x, g y)

    val compareOutputs = fn output_refsol => fn output_submission =>
      if Output.equal (output_refsol,output_submission)
        then NONE
        else SOME output_submission

    val mergeOptions = fn ((input_string,_),(output_refsol,output_submission')) =>
      Option.map (fn output_submission => (input_string,output_refsol,output_submission)) output_submission'

    val min = fn
      (NONE  ,y) => y
    | (SOME x,_) => SOME x

    val sequence = fn
      Result.Value x   => Option.map Result.return x
    | Result.Raise e   => SOME (Result.Raise e)
    | Result.Timeout t => SOME (Result.Timeout t)

    val process = fn () =>
      tests
      |> List.map ((Result.valOf o eval refsol) &&& eval submission o #2)
      |> List.map (fn (r',s) => (r', (sequence o Result.map (compareOutputs r')) s))
      |> Fn.curry (ListPair.map mergeOptions) tests
      |> List.foldr min NONE
  end
