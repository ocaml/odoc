open Result

val ast_to_comment :
  permissive:bool ->
  sections_allowed:Ast.sections_allowed ->
  parent_of_sections:Model.Paths.Identifier.label_parent ->
  Ast.docs ->
    ((Model.Comment.docs, Model.Error.t) result) Model.Error.with_warnings
