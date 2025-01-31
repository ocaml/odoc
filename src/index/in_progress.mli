(** Intermediate representation for pages hierarchies to be able to add pages
    before *)

module Id = Odoc_model.Paths.Identifier
open Odoc_model

type in_progress
(** A directory *)

(** {1 Initial value} *)

val empty_t : Id.ContainerPage.t option -> in_progress
(** Start a hierarchy for a parent ID ([None] is for the absolute root) *)

(** {1 Add to the initial value} *)

val add_page : in_progress -> Lang.Page.t -> unit
(** Add a leaf pages in the given dir *)

val add_module : in_progress -> Lang.Compilation_unit.t -> unit
(** Add a mpodule in the given dir *)

val add_implementation : in_progress -> Lang.Implementation.t -> unit
(** Add a mpodule in the given dir *)

(** {1 Getters} *)

val root_dir : in_progress -> Id.ContainerPage.t option
(** [root dir] is the parent ID represented by [dir] *)

val leafs : in_progress -> (Id.LeafPage.t * Lang.Page.t) list
(** [leafs dir] returns the leaf pages in [dir] *)

val dirs : in_progress -> (Id.ContainerPage.t * in_progress) list
(** [dirs dir] returns the intermediate directories in [dir] *)

val modules : in_progress -> (Id.RootModule.t * Skeleton.t) list
(** [modules dir] returns the modules in [dir] *)

val implementations :
  in_progress -> (Id.SourcePage.t * Lang.Implementation.t) list
(** [implementations dir] returns the implementations in [dir] *)

val index : in_progress -> (Id.LeafPage.t * Lang.Page.t) option
(** [index dir] returns the potential [index] leaf page in [dir] *)
