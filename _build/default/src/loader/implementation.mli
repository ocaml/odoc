open Odoc_model

val read_cmt_infos :
  Paths.Identifier.Id.source_page option ->
  (Compat.shape * Compat.uid_to_loc) option ->
  Typedtree.structure ->
  string ->
  Root.t ->
  Lang.Compilation_unit.Import.t list ->
  Lang.Implementation.t
(** Extract all implementation information from a [cmt]: the shape, and the
    {{!Odoc_model.Lang.Source_info.infos}source infos} (local and global
    definitions and occurrences).

    In OCaml version below 4.14, the information is always empty. *)
