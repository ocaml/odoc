val doc_of_locs :
  string ->
  (Types.src_loc * (int * int)) list ->
  [> Html_types.pre ] Tyxml.Html.elt
