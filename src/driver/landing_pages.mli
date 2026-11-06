(** The pages the driver writes itself. They are an index for each package that
    does not ship one, an index for each library, one page for the sources, and
    the top-level list of packages. *)

open Odoc_unit

val make_index :
  dirs:dirs ->
  rel_dir:Fpath.t ->
  obj_dir:Fpath.t ->
  enable_warnings:bool ->
  content:(Format.formatter -> unit) ->
  mld t
(** Write an [index.mld] with the given contents and make its unit. [rel_dir] is
    the page's parent id; [obj_dir] is where its [.odoc] file goes, relative to
    the odoc directory. *)

val library : dirs:dirs -> pkg:Packages.t -> Packages.libty -> mld t
(** The page of a library: the list of its modules. *)

val package : dirs:dirs -> pkg:Packages.t -> mld t
(** The page of a package: its pages and, per library, its modules. *)

val src : dirs:dirs -> pkg:Packages.t -> mld t
(** The page introducing a package's rendered sources. *)

val package_list :
  dirs:dirs ->
  remap:bool ->
  selected:Util.StringSet.t ->
  Packages.t list ->
  mld t
(** The top-level page: the list of documented packages. *)
