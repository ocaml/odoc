val parse :
  containing_definition:Model.Paths.Identifier.label_parent ->
  comment_text:string ->
    (Model.Comment.comment', Model.Error.t) result
