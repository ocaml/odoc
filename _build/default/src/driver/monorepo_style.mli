val monorepo_pkg_name : string

val of_dune_build :
  Fpath.t -> extra_pkgs:string list -> extra_libs:string list -> Packages.t list
