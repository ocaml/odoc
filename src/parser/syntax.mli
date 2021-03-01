val parse :
  Error.warning_accumulator ->
  Token.t Location_.with_location Stream.t ->
  Ast.docs
