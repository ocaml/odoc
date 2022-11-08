val doc_of_locs :
  string ->
  (Types.src_loc * Location.t) list ->
  [> Html_types.pre ] Tyxml.Html.elt
