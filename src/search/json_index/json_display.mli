open Odoc_search

val of_entry :
  Odoc_index.Entry.t ->
  Html.html list ->
  (Odoc_html.Json.json, Odoc_document.Url.Error.t) Result.result
