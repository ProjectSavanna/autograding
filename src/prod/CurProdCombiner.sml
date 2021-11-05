functor CurProdCombiner (
  val descriptions : string list
  val weights : int list
) =
struct
  local
    val fst = fn (x, _) => x
    val snd = fn (_, x) => x

    val total = List.foldr Int.+ 0 weights
    val fractions = List.map (
      case total of
        0 => Fn.const Rational.one
      | _ => Fn.curry (Fn.flip Rational.//) total
    ) weights

    val combine = fn (percent,description) => percent ^ " " ^ description
    val format = fn L => (* todo: better name? *)
      String.concat (
        ListPair.map format (
          ListPair.map combine (
            ListPair.map FormatUtil.showPercents (
              List.map snd L,
              fractions
            ),
            descriptions
          ),
          (List.map fst L)
        )
      )

    val weightedAverage =
      List.foldr Rational.+ Rational.zero
      o Fn.curry (ListPair.map Rational.*) fractions
  in
    val combine = fn L => (* todo: better name? *)
      ( format L, weightedAverage (List.map snd L) )
  end
end
