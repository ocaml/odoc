module Location = Model.Location_
module Error = Model.Error



let bad_markup : string -> Location.span -> Error.t =
  Error.format "'%s': bad markup"

let bad_heading_level : int -> Location.span -> Error.t =
  Error.format "'%d': bad heading level (0-5 allowed)"

let leading_zero_in_heading_level : string -> Location.span -> Error.t =
  Error.format "'%s': leading zero in heading level"

let cannot_be_empty : what:string -> Location.span -> Error.t = fun ~what ->
  Error.format "%s cannot be empty" what

let must_begin_on_its_own_line : what:string -> Location.span -> Error.t =
    fun ~what ->
  Error.format "%s must begin on its own line" what

let must_be_followed_by_whitespace : what:string -> Location.span -> Error.t =
    fun ~what ->
  Error.format "%s must be followed by space, a tab, or a new line" what

let not_allowed
    : ?suggestion:string -> what:string -> in_what:string -> Location.span ->
        Error.t =
    fun ?suggestion ~what ~in_what location ->
  let message = Printf.sprintf "%s is not allowed in %s" what in_what in
  let message =
    match suggestion with
    | None -> message
    | Some suggestion -> Printf.sprintf "%s\nSuggestion: %s" message suggestion
  in
  Error.make message location

let no_leading_whitespace_in_verbatim : Location.span -> Error.t =
  Error.make "'{v' must be followed by whitespace"

let no_trailing_whitespace_in_verbatim : Location.span -> Error.t =
  Error.make "'v}' must be preceded by whitespace"

let page_heading_required : string -> Error.t =
  Error.filename_only "pages (.mld files) must start with a heading"

let heading_level_must_be_lower_than_top_level
    : int -> int -> Location.span -> Error.t =
    fun this_heading_level top_heading_level ->
  Error.format
    "%s: heading level must be lower than top heading level '%d'"
    (Token.print (`Begin_section_heading (this_heading_level, None)))
    top_heading_level

let headings_not_allowed : Location.span -> Error.t =
  Error.make "headings not allowed in this comment"

let titles_not_allowed : Location.span -> Error.t =
  Error.make "title-level headings {0 ...} are only allowed in pages"

let stray_at : Location.span -> Error.t =
  Error.make "stray '@'"

let stray_cr : Location.span -> Error.t =
  Error.make "stray '\\r' (carriage return character)"

let truncated_before : Location.span -> Error.t =
  Error.make "'@before' expects version number on the same line"

let truncated_param : Location.span -> Error.t =
  Error.make "'@param' expects parameter name on the same line"

let truncated_raise : Location.span -> Error.t =
  Error.make "'@raise' expects exception constructor on the same line"

let truncated_see : Location.span -> Error.t =
  Error.make "'@see' must be followed by <url>, 'file', or \"document title\""

let unknown_tag : string -> Location.span -> Error.t =
  Error.format "unknown tag '%s'"

let unpaired_right_brace : Location.span -> Error.t =
  Error.make "unpaired '}' (end of markup)"

let unpaired_right_bracket : Location.span -> Error.t =
  Error.make "unpaired ']' (end of code)"

let invalid_raw_markup_target : string -> Location.span -> Error.t =
  Error.format "'{%%%s:': bad raw markup target"

let default_raw_markup_target_not_supported : Location.span -> Error.t =
  Error.format
    "%s needs a target language, try %s"
    (Token.describe (`Raw_markup (`Html, "")))
    "'{%html:...%}'"
