open Result

val parse :
  (Token.t Model.Location_.with_location) Stream.t ->
    (Ast.docs, Model.Error.t) result
