open Odoc_unit

val library :
  dirs:dirs -> pkg:Packages.t -> index:index -> Packages.libty -> mld unit

val package : dirs:dirs -> pkg:Packages.t -> index:index -> mld unit

val src : dirs:dirs -> pkg:Packages.t -> index:index -> mld unit

val package_list : dirs:dirs -> Packages.t list -> mld unit
