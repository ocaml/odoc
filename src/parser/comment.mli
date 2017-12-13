val comment :
  parent_of_sections:Model.Paths.Identifier.label_parent ->
  token_stream:((int * int) * Token.t) Stream.t ->
    Model.Comment.docs
