val parser_output :
  Format.formatter ->
  ((Model.Comment.docs, Model.Error.t) result) Model.Error.with_warnings ->
    unit
