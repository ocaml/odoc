module Id : sig
  type t
  val to_fpath : t -> Fpath.t
  val of_fpath : Fpath.t -> t
  val to_string : t -> string
end

val index_filename : string
val sidebar_filename : string

val odoc : Bos.Cmd.t ref
val odoc_md : Bos.Cmd.t ref

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
  warnings_tag:string option ->
  parent_id:Id.t ->
  ignore_output:bool ->
  unit
val compile_md :
  output_dir:Fpath.t -> input_file:Fpath.t -> parent_id:Id.t -> unit

val compile_asset : output_dir:Fpath.t -> name:string -> parent_id:Id.t -> unit

val link :
  ?ignore_output:bool ->
  custom_layout:bool ->
  input_file:Fpath.t ->
  ?output_file:Fpath.t ->
  docs:(string * Fpath.t) list ->
  libs:(string * Fpath.t) list ->
  includes:Fpath.t list ->
  warnings_tags:string list ->
  ?current_package:string ->
  unit ->
  unit

val compile_index :
  ?ignore_output:bool ->
  output_file:Fpath.t ->
  ?occurrence_file:Fpath.t ->
  json:bool ->
  roots:Fpath.t list ->
  simplified:bool ->
  wrap:bool ->
  unit ->
  unit

val sidebar_generate :
  ?ignore_output:bool ->
  output_file:Fpath.t ->
  json:bool ->
  Fpath.t ->
  unit ->
  unit

val html_generate :
  output_dir:string ->
  ?sidebar:Fpath.t ->
  ?ignore_output:bool ->
  ?search_uris:Fpath.t list ->
  ?remap:Fpath.t ->
  ?as_json:bool ->
  ?home_breadcrumb:string ->
  input_file:Fpath.t ->
  unit ->
  unit

val html_generate_asset :
  output_dir:string ->
  ?ignore_output:bool ->
  ?home_breadcrumb:string ->
  input_file:Fpath.t ->
  asset_path:Fpath.t ->
  unit ->
  unit

val html_generate_source :
  output_dir:string ->
  ?ignore_output:bool ->
  source:Fpath.t ->
  ?sidebar:Fpath.t ->
  ?search_uris:Fpath.t list ->
  ?as_json:bool ->
  ?home_breadcrumb:string ->
  input_file:Fpath.t ->
  unit ->
  unit

val support_files : Fpath.t -> string list

val count_occurrences : input:Fpath.t list -> output:Fpath.t -> unit
