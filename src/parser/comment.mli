val comment :
  parent_of_sections:Model.Paths.Identifier.label_parent ->
  file:string ->
  offset_to_location:(int -> Model.Location_.point) ->
  token_stream:((int * int) * Token.t) Stream.t ->
    Ast.docs
