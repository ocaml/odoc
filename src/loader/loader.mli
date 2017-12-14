val read_string :
  Model.Paths.Identifier.label_parent ->
  Location.t ->
  string ->
    Model.Comment.comment

type read_result =
  (Model.Lang.Compilation_unit.t, read_error) result

and read_error = private
  | Not_an_interface
  | Wrong_version
  | Corrupted
  | Not_a_typedtree
  | Not_an_implementation

val read_cmti :
  make_root:(module_name:string -> digest:Digest.t -> Model.Root.t) ->
  filename:string ->
    read_result

val read_cmt :
  make_root:(module_name:string -> digest:Digest.t -> Model.Root.t) ->
  filename:string ->
    read_result

val read_cmi :
  make_root:(module_name:string -> digest:Digest.t -> Model.Root.t) ->
  filename:string ->
    read_result
