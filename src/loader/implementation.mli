val of_cmt :
  Odoc_model.Paths.Identifier.SourcePage.t ->
  Odoc_model.Paths.Identifier.RootModule.t ->
  Typedtree.structure -> Warnings.loc Types.Uid.Tbl.t ->
  Odoc_model.Paths.Identifier.Id.source_location Odoc_model.Compat.shape_uid_map * Odoc_model.Lang.Source_info.infos
(** Extract all implementation information from a [cmt]: the shape, and the
    {{!Odoc_model.Lang.Source_info.infos}source infos} (local and global
    definitions and occurrences).

    In OCaml version below 4.14, the information is always empty. *)
