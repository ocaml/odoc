val ast_to_comment :
  permissive:bool -> sections_allowed:Ast.sections_allowed -> Ast.docs ->
    ((Model.Comment.docs, Model.Error.t) result) Model.Error.with_warnings
