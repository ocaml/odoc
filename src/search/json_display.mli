val of_entry : Entry.t -> Odoc_html.Json.json

val of_strings :
  id:string list ->
  url:string ->
  doc:string ->
  kind:string ->
  rhs:string option ->
  Odoc_html.Json.json

val rhs_of_kind : Entry.extra -> string option
