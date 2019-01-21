val parse_comment :
  sections_allowed:Ast.sections_allowed ->
  containing_definition:Model.Paths.Identifier.LabelParent.t ->
  location:Lexing.position ->
  text:string ->
    Model.Comment.docs Model.Error.with_warnings
