open Odoc_model
open Odoc_utils

(** Page hierarchies represent a hierarchy of pages. *)

type title = Comment.link_content

type t = Entry.t Tree.t

val of_list :
  pages:Lang.Page.t list -> modules:Lang.Compilation_unit.t list -> t
(** Uses the convention that the [index] children passes its payload to the
      container directory to output a payload *)
