open Odoc_unit

val make_index :
  dirs:dirs ->
  rel_dir:Fpath.t ->
  libs:(Packages.t * Packages.libty) list ->
  pkgs:Packages.t list ->
  index:index option ->
  enable_warnings:bool ->
  content:(Format.formatter -> unit) ->
  mld Odoc_unit.t

val library :
  dirs:dirs -> pkg:Packages.t -> index:index -> Packages.libty -> mld t

val package : dirs:dirs -> pkg:Packages.t -> index:index -> mld t

val src : dirs:dirs -> pkg:Packages.t -> index:index -> mld t

val package_list : dirs:dirs -> remap:bool -> Packages.t list -> mld t

val make_custom :
  dirs -> (Packages.t -> Odoc_unit.index) -> Packages.t -> mld t list
