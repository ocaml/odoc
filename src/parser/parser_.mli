val parse :
  containing_definition:Model.Paths.Identifier.label_parent ->
  comment_text:string ->
    (Model.Comment.docs, Model.Error.t) result
