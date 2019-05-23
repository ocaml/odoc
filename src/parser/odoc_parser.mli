val parse_comment :
  sections_allowed:Ast.sections_allowed ->
  containing_definition:Odoc_model.Paths.Identifier.LabelParent.t ->
  location:Lexing.position ->
  text:string ->
    Odoc_model.Comment.docs Odoc_model.Error.with_warnings
