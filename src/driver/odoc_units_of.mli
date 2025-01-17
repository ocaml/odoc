open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

val packages :
  dirs:dirs ->
  extra_paths:Voodoo.extra_paths ->
  remap:bool ->
  indices_style:indices_style ->
  Packages.t list ->
  any list
