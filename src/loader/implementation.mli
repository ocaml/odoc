val of_cmt :
  Odoc_model.Paths.Identifier.SourcePage.t option ->
  Odoc_model.Paths.Identifier.RootModule.t ->
  Cmt_format.cmt_infos ->
  Shape_.t option * Odoc_model.Lang.Source_info.infos
(** Extract all implementation information from a [cmt]: the shape, and the
    {{!Odoc_model.Lang.Source_info.infos}source infos} (local and global
    definitions and occurrences).

    In OCaml version below 4.14, the information is always empty. *)
