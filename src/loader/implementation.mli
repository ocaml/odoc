val of_cmt :
  Odoc_model.Paths.Identifier.SourcePage.t option ->
  Odoc_model.Paths.Identifier.RootModule.t ->
  Cmt_format.cmt_infos ->
  Shape_.t option * Odoc_model.Lang.Source_info.infos
