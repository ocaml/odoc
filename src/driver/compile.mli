(** The steps that turn a package's units into HTML. Each function runs the odoc
    commands for one library or one package; {!Build} decides the order. *)

val init_stats : Odoc_unit.pkg list -> unit
(** Tell the progress display how much there is to do. *)

val compile_lib : Odoc_unit.lib -> unit
(** Compile the modules of a library, each module after the modules it depends
    on. Interfaces go through [odoc compile] and implementations through
    [odoc compile-impl], all with the library and its transitive dependencies as
    [-L]. All dependencies in other libraries must have been compiled already;
    odoc finds them there. *)

val compile_pages : Odoc_unit.pkg -> unit
(** Compile the pages and assets of a package, and the page of each of its
    libraries. *)

val link : warnings_tags:string list -> Odoc_unit.pkg -> unit
(** [odoc link] the units of a package with the package's scope. Everything the
    scope names must have been compiled. Warnings are reported for the packages
    in [warnings_tags]. *)

val html_support : Fpath.t -> unit
(** Write the files every page relies on into the HTML directory. *)

val with_remaps : (string * string) list -> (Fpath.t option -> 'a) -> 'a
(** [with_remaps remaps f] calls [f] with a file describing the link
    redirections, for {!generate}, or with [None] when there are none. *)

val generate :
  ?remap_file:Fpath.t -> generate_json:bool -> Fpath.t -> Odoc_unit.pkg -> unit
(** Build a package's index, sidebar and search database, then render its units
    as HTML into the given directory. *)

val json_index : occurrence_file:Fpath.t -> Fpath.t -> Odoc_unit.pkg -> unit
(** Write a package's search index as JSON ([index.js]) into the HTML directory,
    with the occurrence counts from [occurrence_file]. This is the only step
    that uses occurrence counts. *)
