open Odoc_model
open Paths.Identifier

type library = { name : string; units : RootModule.t list }

type page_hierarchy = { hierarchy_name : string; pages : Page_hierarchy.t }

type t = { pages : page_hierarchy list; libraries : library list }
