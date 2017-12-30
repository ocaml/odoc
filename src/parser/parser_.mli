val parse_comment :
  permissive:bool ->
  [ `Allow_all_sections | `No_titles_allowed | `No_sections ] ->
  containing_definition:Model.Paths.Identifier.label_parent ->
  location:Lexing.position ->
  text:string ->
    ((Model.Comment.docs, Model.Error.t) result) Model.Error.with_warnings
