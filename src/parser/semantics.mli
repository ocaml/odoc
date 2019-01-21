val ast_to_comment :
  Model.Error.warning_accumulator ->
  sections_allowed:Ast.sections_allowed ->
  parent_of_sections:Model.Paths.Identifier.LabelParent.t ->
  Ast.docs ->
    Model.Comment.docs
