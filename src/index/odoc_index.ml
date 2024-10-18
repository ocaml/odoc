open Odoc_model

module Skeleton = Skeleton
module Entry = Entry
module Page_hierarchy = Page_hierarchy

type title = Comment.link_content
type page = { p_name : string; p_hierarchy : Page_hierarchy.t }

type lib_hierarchies = Skeleton.t list
type lib = { l_name : string; l_hierarchies : lib_hierarchies }

type t = {
  pages : page list;
  libs : lib list;
  extra : Skeleton.t list;
      (** This extra table is used only for search. It was introduced before
          Odoc 3 *)
}
