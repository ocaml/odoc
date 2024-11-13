open Odoc_model
open Odoc_utils

(** Page hierarchies represent a hierarchy of pages. *)

type title = Comment.link_content

type index =
  | Page of Paths.Identifier.Page.t * title
  | Missing_index of Paths.Identifier.ContainerPage.t option

type t = index Tree.t

val of_list : Lang.Page.t list -> t
(** Uses the convention that the [index] children passes its payload to the
      container directory to output a payload *)
