open Odoc_unit

val packages :
  dirs:dirs ->
  extra_paths:Voodoo.extra_paths ->
  remap:bool ->
  Packages.t list ->
  t list
