(** From what is installed to what to build.

    This module decides, for every package, the units to compile. It decides the
    identifier and the location of each unit, the transitive dependencies of
    each library, and the reference scope of the package. All of it is derived
    from names: what opam and findlib say about the switch, and the layout
    described in {!Odoc_unit}. *)

(** How the top-level pages are made. *)
type indices_style =
  | Voodoo  (** No top-level page: ocaml.org provides its own. *)
  | Normal of { toplevel_content : string option }
      (** A top-level page listing the packages, or the given [.mld] contents
          instead. *)

val packages :
  installed:Installed.t ->
  dirs:Odoc_unit.dirs ->
  remap:bool ->
  selected:Util.StringSet.t ->
  indices_style:indices_style ->
  Packages.t list ->
  Odoc_unit.pkg list
(** The packages to build, in the order given. [selected] names the packages the
    run was asked for, rather than pulled in as dependencies: warnings are
    reported for those, and only those are rendered. With [Normal] indices the
    last element is the top-level index, which belongs to no package. With
    [remap], the units of the packages that are not selected are compiled but
    not rendered, and links into them are redirected (see {!Packages.remaps}).
*)
