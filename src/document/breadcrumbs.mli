open Odoc_model
module Id = Paths.Identifier

type t = (string * Url.Path.t) list

val of_lang : index:'a Lang.Index.t option -> Id.OdocId.t -> t
(** Compute the breadcrumbs for the current unit. If [index] is not [None],
    it's used to retrieve the short titles of pages. *)
