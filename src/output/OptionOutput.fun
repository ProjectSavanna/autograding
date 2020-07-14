functor OptionOutput (Output : OUTPUT) :> OUTPUT where type t = Output.t option =
  struct
    type t = Output.t option
    val toString = fn
      NONE   => "NONE"
    | SOME x => (
        let
          val s = Output.toString x
        in
          case String.isPrefix "(" s of
            false => "SOME (" ^ s ^ ")"
          | true  => "SOME " ^ s
        end
      )
    val equal = fn
      (NONE  ,NONE  ) => true
    | (SOME _,NONE  ) => false
    | (NONE  ,SOME _) => false
    | (SOME x,SOME y) => Output.equal (x,y)
  end
