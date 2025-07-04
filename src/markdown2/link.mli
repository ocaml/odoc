(** Markdown-specific interpretation of {!Odoc_document.Url} *)

type resolve = Current of Odoc_document.Url.Path.t | Base of string

val href : config:Config.t -> resolve:resolve -> Odoc_document.Url.t -> string

module Path : sig
  val is_leaf_page : Odoc_document.Url.Path.t -> bool

  val for_printing : Odoc_document.Url.Path.t -> string list

  val as_filename : config:Config.t -> Odoc_document.Url.Path.t -> Fpath.t
end
