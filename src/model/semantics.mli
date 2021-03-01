val ast_to_comment :
  Odoc_parser.Error.warning_accumulator ->
  sections_allowed:Odoc_parser.Ast.sections_allowed ->
  parent_of_sections:Paths.Identifier.LabelParent.t ->
  Odoc_parser.Ast.docs ->
  Comment.docs

val parse_comment :
  sections_allowed:Odoc_parser.Ast.sections_allowed ->
  containing_definition:Paths.Identifier.LabelParent.t ->
  location:Lexing.position ->
  text:string ->
  Comment.docs Error.with_warnings

val parse_reference :
  string -> (Paths.Reference.t, [> `Msg of string ]) Result.result
