functor BucketGrader (I : BUCKET_INPUT) =
  GeneralizedBucketGrader (
    open I

    structure Scheme =
      struct
        type 'a t = 'a option

        fun aggregate (f : 'a -> 'b option) : 'a list -> 'b t = fn
          nil     => NONE
        | x :: xs => (
            case f x of
              NONE   => aggregate f xs
            | SOME y => SOME y
          )

        structure Output = Output

        val score = fn
          NONE   => Rational.one
        | SOME _ => Rational.zero

        val toString = fn f => fn
          NONE   => "All tests passed.\n"
        | SOME x => f x
      end
  )

functor BucketGraderList (
  I :
    sig
      include BUCKET_INPUT
      val cutoff : int
    end
) =
  GeneralizedBucketGrader (
    open I

    structure Scheme =
      struct
        type 'a t = 'a list * int

        fun aggregate (f : 'a -> 'b option) : 'a list -> 'b t = fn
          nil     => (nil, 0)
        | x :: xs => (
            let
              val (l, n) = aggregate f xs
            in
              case f x of
                NONE   => (l, n + 1)
              | SOME y => (y :: l, n)
            end
          )

        structure Output = Output

        val score = fn (l, n) => Rational.// (n, n + List.length l)

        val toString = fn f => fn (l, n) =>
          "Tests passed: " ^ Int.toString n ^ "/" ^ Int.toString (n + List.length l) ^ " (" ^ Rational.percent (score (l, n)) ^ ")\n" ^
          String.concat (
            List.map
              (fn failedCase => FormatUtil.indentWith #"-" (f failedCase))
              (List.take (l, Int.min (List.length l, cutoff)))
          ) ^ (if List.length l > cutoff then "- ...\n" else "")
      end
  )
