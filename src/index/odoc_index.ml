module Entry = Entry
module Fold = Fold
module Sidebar = Sidebar

type 'a t = {
  sidebar : Sidebar.t;
  index : 'a Odoc_model.Paths.Identifier.Hashtbl.Any.t;
}
