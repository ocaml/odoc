(** Running [odoc]. Each function builds one command line and runs it on the
    {!Worker_pool}. See the driver documentation for what each command does. *)

(** Identifiers of units. An identifier is the parent id given to
    [odoc compile], a path such as [fmt/fmt]. It decides how references to the
    unit resolve, and where its HTML goes. *)
module Id : sig
  type t

  val of_fpath : Fpath.t -> t
  val to_fpath : t -> Fpath.t
  val to_string : t -> string
end

val odoc : Bos.Cmd.t ref
(** The [odoc] command. Drivers may replace it, for example with a path. *)

val odoc_md : Bos.Cmd.t ref
(** The [odoc-md] command, which compiles Markdown files. *)

val index_filename : string
val sidebar_filename : string

(** {1 Inspecting inputs} *)

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

val compile_deps : Fpath.t -> (compile_deps, [> `Msg of string ]) result
(** [odoc compile-deps]: the digest of a compiled module's interface and the
    names and digests of the modules it depends on. *)

val classify : Fpath.t list -> (string * string list) list
(** [odoc classify]: for each archive found in the given directories, the names
    of its modules. *)

(** {1 Compiling} *)

val compile :
  output_file:Fpath.t ->
  input_file:Fpath.t ->
  libs:(string * Fpath.t) list ->
  warnings_tag:string option ->
  parent_id:Id.t ->
  ignore_output:bool ->
  Cmd_outputs.action
(** [odoc compile] of an interface or a page. [parent_id] decides the unit's
    identifier. [output_file] is where the [.odoc] file goes, and it need not
    lie below the parent id. [libs] are the libraries the input was built
    against, each with the directory of its [.odoc] files. Those directories are
    searched for the modules the input depends on, and the library names are
    written into the unit. *)

val compile_impl :
  output_file:Fpath.t ->
  input_file:Fpath.t ->
  libs:(string * Fpath.t) list ->
  parent_id:Id.t ->
  source_id:Id.t ->
  Cmd_outputs.action
(** [odoc compile-impl] of an implementation. [source_id] is the identifier of
    the rendered source page. *)

val compile_md :
  output_file:Fpath.t ->
  input_file:Fpath.t ->
  parent_id:Id.t ->
  Cmd_outputs.action
(** [odoc-md] of a Markdown file. [output_file] is where the [.odoc] file goes
    and [parent_id] decides the page's identifier; the two are independent. *)

val compile_asset :
  output_file:Fpath.t -> name:string -> parent_id:Id.t -> Cmd_outputs.action
(** [odoc compile-asset]: the unit standing for an asset, likewise. *)

(** {1 Linking} *)

val link :
  ignore_output:bool ->
  input_file:Fpath.t ->
  output_file:Fpath.t ->
  docs:(string * Fpath.t) list ->
  libs:(string * Fpath.t) list ->
  warnings_tags:string list ->
  ?current_package:string ->
  unit ->
  Cmd_outputs.action
(** [odoc link]. [docs] are the page roots ([-P]) and [libs] the library roots
    ([-L]): those the unit may refer to, and those holding the [.odoc] files of
    the modules it depends on. Roots may share a directory, so [--custom-layout]
    is always passed. Warnings are reported for the packages named in
    [warnings_tags]. *)

(** {1 Indexing} *)

val compile_index :
  output_file:Fpath.t ->
  ?occurrence_file:Fpath.t ->
  json:bool ->
  file_list:Fpath.t ->
  simplified:bool ->
  wrap:bool ->
  unit ->
  Cmd_outputs.action
(** [odoc compile-index] over the [.odocl] files listed, one per line, in
    [file_list]. Passing the files rather than directories puts them all in one
    hierarchy. [occurrence_file] is only used for JSON output. *)

val sidebar_generate :
  output_file:Fpath.t -> json:bool -> Fpath.t -> unit -> Cmd_outputs.action
(** [odoc sidebar-generate] from an index. *)

val count_occurrences :
  input:Fpath.t list -> output:Fpath.t -> Cmd_outputs.action
(** [odoc count-occurrences]: how often each item is used by the implementations
    found below the input directories. *)

(** {1 Generating} *)

val html_generate :
  output_dir:string ->
  ?sidebar:Fpath.t ->
  ?search_uris:Fpath.t list ->
  ?remap:Fpath.t ->
  ?as_json:bool ->
  home_breadcrumb:string ->
  input_file:Fpath.t ->
  unit ->
  Cmd_outputs.action
(** [odoc html-generate] of an interface or a page. [remap] is a file of link
    prefixes to rewrite, for links into packages documented elsewhere. *)

val html_generate_source :
  output_dir:string ->
  source:Fpath.t ->
  ?sidebar:Fpath.t ->
  ?search_uris:Fpath.t list ->
  ?as_json:bool ->
  home_breadcrumb:string ->
  input_file:Fpath.t ->
  unit ->
  Cmd_outputs.action
(** [odoc html-generate-source]: the rendered source of an implementation. *)

val html_generate_asset :
  output_dir:string ->
  home_breadcrumb:string ->
  input_file:Fpath.t ->
  asset_path:Fpath.t ->
  unit ->
  Cmd_outputs.action
(** [odoc html-generate-asset]: copy an asset to its place in the output. *)

val support_files : Fpath.t -> Cmd_outputs.action
(** [odoc support-files]: the stylesheets and scripts every page needs. *)
