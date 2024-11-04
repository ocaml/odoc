module Entry = Entry
module Fold = Fold
module Page_hierarchy = Page_hierarchy

type page = { p_name : string; p_hierarchy : Page_hierarchy.t }

type library = {
  name : string;
  units : Odoc_model.Paths.Identifier.RootModule.t list;
}

type sidebar = { pages : page list; libs : library list }

type 'a t = {
  sidebar : sidebar;
  index : 'a Odoc_model.Paths.Identifier.Hashtbl.Any.t;
}
