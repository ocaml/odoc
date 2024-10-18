open Odoc_search
open Odoc_index

val of_entry :
  Entry.t ->
  Html.html list ->
  (Odoc_html.Json.json, Odoc_document.Url.Error.t) Result.result
