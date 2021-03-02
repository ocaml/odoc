open Odoc_compat

let bad_markup : ?suggestion:string -> string -> Location.span -> Error.t =
 fun ?suggestion -> Error.make ?suggestion "'%s': bad markup."

let leading_zero_in_heading_level : string -> Location.span -> Error.t =
  Error.make "'%s': leading zero in heading level."

let should_not_be_empty : what:string -> Location.span -> Error.t =
 fun ~what ->
  Error.make "%s should not be empty." (String.capitalize_ascii what)

let should_begin_on_its_own_line : what:string -> Location.span -> Error.t =
 fun ~what ->
  Error.make "%s should begin on its own line." (String.capitalize_ascii what)

let should_be_followed_by_whitespace : what:string -> Location.span -> Error.t =
 fun ~what ->
  Error.make "%s should be followed by space, a tab, or a new line."
    (String.capitalize_ascii what)

let not_allowed :
    ?suggestion:string ->
    what:string ->
    in_what:string ->
    Location.span ->
    Error.t =
 fun ?suggestion ~what ~in_what ->
  Error.make ?suggestion "%s is not allowed in %s."
    (String.capitalize_ascii what)
    in_what

let no_leading_whitespace_in_verbatim : Location.span -> Error.t =
  Error.make "'{v' should be followed by whitespace."

let no_trailing_whitespace_in_verbatim : Location.span -> Error.t =
  Error.make "'v}' should be preceded by whitespace."

let stray_at : Location.span -> Error.t = Error.make "Stray '@'."

let stray_cr : Location.span -> Error.t =
  Error.make "Stray '\\r' (carriage return character)."

let truncated_before : Location.span -> Error.t =
  Error.make "'@before' expects version number on the same line."

let truncated_param : Location.span -> Error.t =
  Error.make "'@param' expects parameter name on the same line."

let truncated_raise : string -> Location.span -> Error.t =
  Error.make "'%s' expects exception constructor on the same line."

let truncated_see : Location.span -> Error.t =
  Error.make
    "'@see' should be followed by <url>, 'file', or \"document title\"."

let unknown_tag : string -> Location.span -> Error.t =
  Error.make "Unknown tag '%s'."

let unpaired_right_brace : Location.span -> Error.t =
  Error.make ~suggestion:"try '\\}'." "Unpaired '}' (end of markup)."

let unpaired_right_bracket : Location.span -> Error.t =
  Error.make ~suggestion:"try '\\]'." "Unpaired ']' (end of code)."

let expected : string -> Location.span -> Error.t = Error.make "Expected %s."

let unknown_reference_qualifier : string -> Location.span -> Error.t =
  Error.make "Unknown reference qualifier '%s'."

let deprecated_reference_kind : string -> string -> Location.span -> Error.t =
  Error.make "'%s' is deprecated, use '%s' instead."

let reference_kinds_do_not_match : string -> string -> Location.span -> Error.t
    =
  Error.make "Old-style reference kind ('%s:') does not match new ('%s-')."
