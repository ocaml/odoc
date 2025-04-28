(* Type definitions for the Markdown renderer *)

type uri = Absolute of string | Relative of Odoc_document.Url.Path.t option

type file_uri = Absolute of string | Relative of Odoc_document.Url.Path.t
