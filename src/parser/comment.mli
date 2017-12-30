val comment :
  permissive:bool ->
  [ `Allow_all_sections | `No_titles_allowed | `No_sections ] ->
  parent_of_sections:Model.Paths.Identifier.label_parent ->
  token_stream:((int * int) * Token.t) Stream.t ->
  accumulated_warnings:(Helpers.raw_parse_error list) ref ->
    Model.Comment.docs
