val render :
  config:Config.t ->
  sidebar:Odoc_document.Sidebar.t option ->
  Odoc_document.Types.Document.t ->
  Odoc_document.Renderer.page list

val filepath : config:Config.t -> Odoc_document.Url.Path.t -> Fpath.t

val items :
  config:Config.t ->
  resolve:Link.resolve ->
  Odoc_document.Types.Item.t list ->
  Cmarkit.Block.t list

val doc :
  config:Config.t ->
  xref_base_uri:string ->
  Odoc_document.Types.Block.t ->
  Cmarkit.Doc.t

val inline :
  config:Config.t ->
  xref_base_uri:string ->
  Odoc_document.Types.Inline.t ->
  Cmarkit.Inline.t list
