type sections_allowed = Ast.sections_allowed

val parse_comment :
  permissive:bool ->
  sections_allowed:sections_allowed ->
  containing_definition:Model.Paths.Identifier.label_parent ->
  location:Lexing.position ->
  text:string ->
    ((Model.Comment.docs, Model.Error.t) result) Model.Error.with_warnings
