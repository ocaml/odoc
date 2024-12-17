open Odoc_unit

val library :
  dirs:dirs -> pkg:Packages.t -> index:index -> Packages.libty -> mld unit

val package : dirs:dirs -> pkg:Packages.t -> index:index -> mld unit

val src : dirs:dirs -> pkg:Packages.t -> index:index -> mld unit

val package_list : dirs:dirs -> remap:bool -> Packages.t list -> mld unit

val make_custom : dirs -> (Packages.t -> Odoc_unit.index) -> Packages.t ->  mld unit list
