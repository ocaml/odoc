val of_packages :
  mld_dir:Fpath.t ->
  odoc_dir:Fpath.t ->
  odocl_dir:Fpath.t ->
  output_dir:Fpath.t ->
  Packages.t list ->
  (Fpath.t * string) list * [ `Mld ] Odoc_unit.unit list
