let capitalize_ascii = Astring.String.Ascii.capitalize

let bad_markup : ?suggestion:string -> string -> Location.span -> Warning.t =
 fun ?suggestion -> Warning.make ?suggestion "'%s': bad markup."

let leading_zero_in_heading_level : string -> Location.span -> Warning.t =
  Warning.make "'%s': leading zero in heading level."

let should_not_be_empty : what:string -> Location.span -> Warning.t =
 fun ~what -> Warning.make "%s should not be empty." (capitalize_ascii what)

let markup_should_not_be_used : what:string -> Location.span -> Warning.t =
 fun ~what ->
  Warning.make "%s should not be used because it has no effect."
    (capitalize_ascii what)

let should_begin_on_its_own_line : what:string -> Location.span -> Warning.t =
 fun ~what ->
  Warning.make "%s should begin on its own line." (capitalize_ascii what)

let should_be_followed_by_whitespace : what:string -> Location.span -> Warning.t
    =
 fun ~what ->
  Warning.make "%s should be followed by space, a tab, or a new line."
    (capitalize_ascii what)

let not_allowed :
    ?suggestion:string ->
    what:string ->
    in_what:string ->
    Location.span ->
    Warning.t =
 fun ?suggestion ~what ~in_what ->
  Warning.make ?suggestion "%s is not allowed in %s." (capitalize_ascii what)
    in_what

let no_leading_whitespace_in_verbatim : Location.span -> Warning.t =
  Warning.make "'{v' should be followed by whitespace."

let no_trailing_whitespace_in_verbatim : Location.span -> Warning.t =
  Warning.make "'v}' should be preceded by whitespace."

let stray_at : Location.span -> Warning.t = Warning.make "Stray '@'."

let stray_cr : Location.span -> Warning.t =
  Warning.make "Stray '\\r' (carriage return character)."

let truncated_before : Location.span -> Warning.t =
  Warning.make "'@before' expects version number on the same line."

let truncated_param : Location.span -> Warning.t =
  Warning.make "'@param' expects parameter name on the same line."

let truncated_raise : string -> Location.span -> Warning.t =
  Warning.make "'%s' expects exception constructor on the same line."

let truncated_see : Location.span -> Warning.t =
  Warning.make
    "'@see' should be followed by <url>, 'file', or \"document title\"."

let unknown_tag : string -> Location.span -> Warning.t =
  Warning.make "Unknown tag '%s'."

let unpaired_right_brace : Location.span -> Warning.t =
  Warning.make ~suggestion:"try '\\}'." "Unpaired '}' (end of markup)."

let unpaired_right_bracket : Location.span -> Warning.t =
  Warning.make ~suggestion:"try '\\]'." "Unpaired ']' (end of code)."
