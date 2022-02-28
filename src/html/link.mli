(** HTML-specific interpretation of {!Odoc_document.Url} *)

module Url = Odoc_document.Url

type resolve = Current of Url.Path.t | Base of string

val href : config:Config.t -> resolve:resolve -> Url.t -> string

module Path : sig
  val is_leaf_page : Url.Path.t -> bool

  val for_printing : Url.Path.t -> string list

  val for_linking : is_flat:bool -> Url.Path.t -> string list

  val as_filename : is_flat:bool -> Url.Path.t -> Fpath.t
end
