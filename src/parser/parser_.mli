val parse_comment :
  containing_definition:Model.Paths.Identifier.label_parent ->
  location:Lexing.position ->
  text:string ->
    (Model.Comment.docs, Model.Error.t) result
