(** How to handle internal tags. *)
type _ handle_internal_tags =
  | Expect_status
      : [ `Default | `Inline | `Open | `Closed ] handle_internal_tags
  | Expect_canonical
      : [ `Dot of Paths.Path.Module.t * string ] option handle_internal_tags
  | Expect_none : unit handle_internal_tags

type sections_allowed = [ `All | `No_titles | `None ]

type alerts =
  [ `Tag of [ `Alert of string * string option ] ] Location_.with_location list

val ast_to_comment :
  internal_tags:'tags handle_internal_tags ->
  sections_allowed:sections_allowed ->
  tags_allowed:bool ->
  parent_of_sections:Paths.Identifier.LabelParent.t ->
  Odoc_parser.Ast.t ->
  alerts ->
  (Comment.docs * 'tags) Error.with_warnings

val parse_comment :
  internal_tags:'tags handle_internal_tags ->
  sections_allowed:sections_allowed ->
  tags_allowed:bool ->
  containing_definition:Paths.Identifier.LabelParent.t ->
  location:Lexing.position ->
  text:string ->
  (Comment.docs * 'tags) Error.with_warnings

val parse_reference : string -> Paths.Reference.t Error.with_errors_and_warnings
