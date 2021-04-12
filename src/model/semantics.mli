type canonical_path = [ `Root of string | `Dot of Paths.Path.Module.t * string ]

type internal_tag = [ `Canonical of canonical_path | `Inline | `Open | `Closed ]

type internal_tags = internal_tag Comment.with_location list

val ast_to_comment :
  Error.warning_accumulator ->
  sections_allowed:Odoc_parser.Ast.sections_allowed ->
  parent_of_sections:Paths.Identifier.LabelParent.t ->
  Odoc_parser.Ast.docs ->
  Comment.docs * internal_tags

val parse_comment :
  sections_allowed:Odoc_parser.Ast.sections_allowed ->
  containing_definition:Paths.Identifier.LabelParent.t ->
  location:Lexing.position ->
  text:string ->
  (Comment.docs * internal_tags) Error.with_warnings

val parse_reference :
  string -> (Paths.Reference.t, [> `Msg of string ]) Result.result
