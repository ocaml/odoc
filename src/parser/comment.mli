val comment :
  file:string ->
  offset_to_location:(int -> Model.Location_.point) ->
  token_stream:((int * int) * Token.t) Stream.t ->
    Ast.docs
