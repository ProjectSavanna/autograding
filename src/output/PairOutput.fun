functor PairOutput (
  structure A : OUTPUT
  structure B : OUTPUT
) :> OUTPUT where type t = A.t * B.t =
  struct
    type t = A.t * B.t
    val toString = fn (a,b) => "(" ^ A.toString a ^ ", " ^ B.toString b ^ ")"
    val equal = fn ((a,b),(a',b')) => A.equal (a,a') andalso B.equal (b,b')
  end
