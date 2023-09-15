val read_cmt_infos :
  Odoc_model.Paths.Identifier.Id.source_page option ->
  Odoc_model.Paths.Identifier.Id.root_module ->
  Cmt_format.cmt_infos ->
  (Odoc_model.Compat.shape
  * Odoc_model.Paths.Identifier.Id.source_location
    Odoc_model.Compat.shape_uid_map)
  option
  * Odoc_model.Lang.Source_info.t option
(** Extract all implementation information from a [cmt]: the shape, and the
    {{!Odoc_model.Lang.Source_info.infos}source infos} (local and global
    definitions and occurrences).

    In OCaml version below 4.14, the information is always empty. *)
