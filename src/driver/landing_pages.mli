open Odoc_unit

val library :
  dirs:dirs ->
  pkg:Packages.t ->
  index:index option ->
  Packages.libty ->
  mld unit

val package_list : dirs:dirs -> Packages.t list -> mld unit
