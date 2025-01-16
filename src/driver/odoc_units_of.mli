open Odoc_unit

type indices_style = Voodoo | Normal | Automatic

val packages :
  dirs:dirs ->
  extra_paths:Voodoo.extra_paths ->
  remap:bool ->
  indices_style:indices_style ->
  Packages.t list ->
  t list
