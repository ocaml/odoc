open Result

val read_string :
  Model.Paths.Identifier.LabelParent.t ->
  Location.t ->
  string ->
    (Model.Comment.docs_or_stop, Model.Error.t) result

val read_cmti :
  make_root:(module_name:string -> digest:Digest.t -> Model.Root.t) ->
  filename:string ->
    (Model.Lang.Compilation_unit.t, Model.Error.t) result

val read_cmt :
  make_root:(module_name:string -> digest:Digest.t -> Model.Root.t) ->
  filename:string ->
    (Model.Lang.Compilation_unit.t, Model.Error.t) result

val read_cmi :
  make_root:(module_name:string -> digest:Digest.t -> Model.Root.t) ->
  filename:string ->
    (Model.Lang.Compilation_unit.t, Model.Error.t) result
