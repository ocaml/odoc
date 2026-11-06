(** The odoc commands for each unit, at each step.

    Deciding a command and running it are separate steps: {!Compile} runs these,
    and what waits for what is left to {!Build}. *)

val compile_module :
  Odoc_unit.lib -> Odoc_unit.module_unit -> Cmd_outputs.action list
(** Compile an interface, and copy its input where {!Odoc_unit.t.input_copy}
    says; or compile an implementation. *)

val compile_page : Odoc_unit.page -> Cmd_outputs.action
(** Compile a page, a Markdown file or an asset. *)

val link :
  warnings_tags:string list ->
  Odoc_unit.pkg ->
  Odoc_unit.any ->
  Cmd_outputs.action
(** Link a unit of the package with the package's scope. *)

val index :
  html_dir:Fpath.t ->
  file_list:Fpath.t ->
  Odoc_unit.index ->
  Cmd_outputs.action * Cmd_outputs.action list
(** Build a package's index from the [.odocl] files listed in [file_list]. The
    first command writes the index. The others read it, and write the sidebar,
    the JSON sidebar and the search database. *)

val generate :
  html_dir:Fpath.t ->
  ?remap_file:Fpath.t ->
  generate_json:bool ->
  Odoc_unit.pkg ->
  Odoc_unit.any ->
  Cmd_outputs.action list
(** Render a unit as HTML and, with [generate_json], as JSON as well. A unit
    that is not {!Odoc_unit.is_output} needs no command. *)
