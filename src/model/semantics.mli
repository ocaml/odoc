(** How to handle internal tags. *)
type _ handle_internal_tags =
  | Expect_status
      : [ `Default | `Inline | `Open | `Closed ] handle_internal_tags
  | Expect_canonical
      : [ `Dot of Paths.Path.Module.t * string ] option handle_internal_tags
  | Expect_none : unit handle_internal_tags

val ast_to_comment :
  Error.warning_accumulator ->
  internal_tags:'tags handle_internal_tags ->
  sections_allowed:Octavius.Ast.sections_allowed ->
  parent_of_sections:Paths.Identifier.LabelParent.t ->
  Octavius.Ast.docs ->
  Comment.docs * 'tags

val parse_comment :
  internal_tags:'tags handle_internal_tags ->
  sections_allowed:Octavius.Ast.sections_allowed ->
  containing_definition:Paths.Identifier.LabelParent.t ->
  location:Lexing.position ->
  text:string ->
  (Comment.docs * 'tags) Error.with_warnings

val parse_reference :
  string -> (Paths.Reference.t, [> `Msg of string ]) Result.result
