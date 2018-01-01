val comment :
  permissive:bool ->
  [ `Allow_all_sections | `No_titles_allowed | `No_sections ] ->
  parent_of_sections:Model.Paths.Identifier.label_parent ->
  file:string ->
  offset_to_location:(int -> Model.Location_.point) ->
  token_stream:((int * int) * Token.t) Stream.t ->
  accumulated_warnings:(Helpers.raw_parse_error list) ref ->
    Model.Comment.docs
