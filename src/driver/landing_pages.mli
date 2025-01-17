type u = unit

open Odoc_unit

val make_index :
  dirs:dirs ->
  rel_dir:Fpath.t ->
  ?libs:(Packages.t * Packages.libty) list ->
  ?pkgs:Packages.t list ->
  ?index:index ->
  enable_warnings:bool ->
  content:(Format.formatter -> u) ->
  u ->
  [> `Mld ] Odoc_unit.unit

val library :
  dirs:dirs -> pkg:Packages.t -> index:index -> Packages.libty -> mld unit

val package : dirs:dirs -> pkg:Packages.t -> index:index -> mld unit

val src : dirs:dirs -> pkg:Packages.t -> index:index -> mld unit

val package_list : dirs:dirs -> remap:bool -> Packages.t list -> mld unit

val make_custom :
  dirs -> (Packages.t -> Odoc_unit.index) -> Packages.t -> mld unit list
