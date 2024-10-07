module Id : sig
  type t
  val to_fpath : t -> Fpath.t
  val of_fpath : Fpath.t -> t
  val to_string : t -> string
end

val index_filename : string

val odoc : Bos.Cmd.t ref

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }
val compile_deps : Fpath.t -> (compile_deps, [> `Msg of string ]) result
val classify : Fpath.t list -> (string * string list) list
val compile_impl :
  output_dir:Fpath.t ->
  input_file:Fpath.t ->
  includes:Fpath.set ->
  parent_id:Id.t ->
  source_id:Id.t ->
  unit
val compile :
  output_dir:Fpath.t ->
  input_file:Fpath.t ->
  includes:Fpath.set ->
  parent_id:Id.t ->
  unit

val compile_asset : output_dir:Fpath.t -> name:string -> parent_id:Id.t -> unit

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
  ?occurrence_file:Fpath.t ->
  json:bool ->
  docs:(string * Fpath.t) list ->
  libs:(string * Fpath.t) list ->
  unit ->
  unit

val html_generate :
  output_dir:string ->
  ?index:Fpath.t ->
  ?ignore_output:bool ->
  ?search_uris:Fpath.t list ->
  input_file:Fpath.t ->
  unit ->
  unit

val html_generate_asset :
  output_dir:string ->
  ?ignore_output:bool ->
  input_file:Fpath.t ->
  asset_path:Fpath.t ->
  unit ->
  unit

val html_generate_source :
  output_dir:string ->
  ?ignore_output:bool ->
  source:Fpath.t ->
  ?search_uris:Fpath.t list ->
  input_file:Fpath.t ->
  unit ->
  unit

val support_files : Fpath.t -> string list

val count_occurrences : input:Fpath.t list -> output:Fpath.t -> unit
val source_tree :
  ?ignore_output:bool -> parent:string -> output:Fpath.t -> Fpath.t -> unit
