val parse :
  Model.Error.warning_accumulator ->
  (Token.t Model.Location_.with_location) Stream.t ->
    Ast.docs
