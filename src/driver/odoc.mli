type id

val fpath_of_id : id -> Fpath.t
val id_of_fpath : Fpath.t -> id

val index_filename : string

val default : string
val odoc : Bos.Cmd.t ref


type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }
val compile_deps : Fpath.t -> (compile_deps, [> `Msg of string ]) result
val classify : Fpath.t -> (string * string list) list
val compile_impl :
  output_dir:Fpath.t ->
  input_file:Fpath.t ->
  includes:Fpath.set ->
  parent_id:id ->
  source_id:id ->
  unit
val compile :
  output_dir:Fpath.t ->
  input_file:Fpath.t ->
  includes:Fpath.set ->
  parent_id:id ->
  unit

val link :
  ?ignore_output:bool ->
  input_file:Fpath.t ->
  ?output_file:Fpath.t ->
  includes:Fpath.set ->
  docs:(string * Fpath.t) list ->
  libs:(string * Fpath.t) list ->
  current_package:string ->
  unit ->
  unit

val compile_index :
  ?ignore_output:bool ->
  output_file:Fpath.t ->
  json:bool ->
  docs:(string * Fpath.t) list ->
  libs:(string * Fpath.t) list ->
  unit ->
  unit

val html_generate :
  output_dir:string ->
  ?index:Fpath.t ->
  ?ignore_output:bool ->
  ?assets:string list ->
  ?source:Fpath.t ->
  ?search_uris:Fpath.t list ->
  input_file:Fpath.t ->
  unit ->
  unit
val support_files : Fpath.t -> string list

val count_occurrences : Fpath.t -> string list
val source_tree :
  ?ignore_output:bool -> parent:string -> output:Fpath.t -> Fpath.t -> unit
