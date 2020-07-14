functor TripleOutput (
  structure A : OUTPUT
  structure B : OUTPUT
  structure C : OUTPUT
) :> OUTPUT where type t = A.t * B.t * C.t =
  struct
    type t = A.t * B.t * C.t
    val toString = fn (a,b,c) => "(" ^ A.toString a ^ ", " ^ B.toString b ^ ", " ^ C.toString c ^ ")"
    val equal = fn ((a,b,c),(a',b',c')) => A.equal (a,a') andalso B.equal (b,b') andalso C.equal (c,c')
  end
