(** HTML-specific interpretation of {!Odoc_document.Url} *)

module Url = Odoc_document.Url

val semantic_uris : bool ref
(** Whether to generate pretty/semantics links or not. *)

type resolve =
  | Current of Url.Path.t
  | Base of string

val href : resolve:resolve -> Url.t -> string

module Path : sig
  val is_leaf_page : Url.Path.t -> bool
  val for_printing : Url.Path.t -> string list
  val for_linking : Url.Path.t -> string list
  val as_filename : Url.Path.t -> Fpath.t
end
