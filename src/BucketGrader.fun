structure Display =
  struct
    type string = unit -> string
    type 'a t = 'a * string  (* thunk *)
  end

functor BucketGrader (
  val description : string

  type input
  structure Output : OUTPUT

  val tests : (input Display.t * (Output.t -> bool) Display.t) list
  val timeout : Time.time

  structure Bucket : OUTPUT
  val bucket : input -> Bucket.t
  val buckets : (Bucket.t * int) list

  val submission : input -> Output.t
) :> GRADER =
  struct
    structure Rubric =
      struct
        val description = description

        type t = (Display.string * Display.string * Output.t Result.t) option list
        (* invariant: length = List.length buckets *)

        val scores : t -> Rational.t list =
          List.map (fn NONE => Rational.one | SOME _ => Rational.zero)

        local
          val weights = List.map #2 buckets
          val total = List.foldr Int.+ 0 weights
        in
          val fractions = List.map (
            case total of
              0 => Fn.const Rational.one
            | _ => Fn.curry (Fn.flip Rational.//) total
          ) weights
        end

        local
          val combine = fn (percent,description) => percent ^ " " ^ description
          val format = fn (description,output) => description ^ "\n" ^ FormatUtil.indent output
          val toString = fn
            NONE => "All tests passed.\n"
          | SOME (input_string,expected_string,bad_output) => (
              "Test failed:\n" ^
              "  Test    : " ^ input_string () ^ "\n" ^
              "  Expected: " ^ expected_string () ^ "\n" ^
              "  Received: " ^ Result.toString Output.toString bad_output ^ "\n"
            )
        in
          val toString = fn
            nil      => "No tests run.\n"
          | x :: nil => toString x
          | rubric =>
            String.concat (
              ListPair.map format (
                ListPair.map combine (
                  ListPair.map FormatUtil.showPercents (
                    scores rubric,
                    fractions
                  ),
                  List.map (Bucket.toString o #1) buckets
                ),
                List.map toString rubric
              )
            )
        end

        val score =
          List.foldr Rational.+ Rational.zero
          o Fn.curry (ListPair.map Rational.* ) fractions
          o scores
      end

    local
      val rec partition = fn p => fn
        nil     => (fn _ => nil)
      | x :: xs => (
          let
            val f = partition p xs
          in
            fn y => if Bucket.equal (y, p x) then x :: f y else f y
          end
        )
      val testBuckets = partition (bucket o #1 o #1) tests

      val eval = fn f => Result.evaluate timeout f

      val rec processBucket = fn
        nil                                        => NONE
      | ((input,inputString),(p,pString)) :: tests =>
          let
            val resultOpt =
              case eval submission input of
                Result.Value x   => if p x then NONE else SOME (Result.Value x)
              | Result.Raise e   => SOME (Result.Raise e)
              | Result.Timeout t => SOME (Result.Timeout t)
          in
            case resultOpt of
              NONE        => processBucket tests
            | SOME result => SOME (inputString, pString, result)
          end
    in
      val process = fn () => List.map (processBucket o testBuckets o #1) buckets
    end
  end
