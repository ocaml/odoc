val parse :
  Error.warning_accumulator ->
  Token.t Location.with_location Stream.t ->
  Ast.docs
