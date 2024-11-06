open Odoc_unit

val packages :
  dirs:dirs ->
  extra_libs_paths:Fpath.t Util.StringMap.t ->
  Packages.t list ->
  t list
