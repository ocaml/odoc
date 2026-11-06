(** Building a set of packages, each as a whole, those that do not depend on one
    another at the same time. *)

val compile_package : Odoc_unit.pkg -> unit
(** Compile one package: its libraries, each after the libraries it requires,
    and then its pages. This is what {!all} does for each package. Voodoo mode
    does it on its own, since ocaml-docs-ci gives that mode one package per job,
    and earlier jobs compiled the dependencies. *)

val all :
  html_dir:Fpath.t ->
  remaps:(string * string) list ->
  generate_json:bool ->
  warnings_tags:string list ->
  Odoc_unit.pkg list ->
  unit
(** Build the packages: compile each, then link and render it.

    A package is compiled once every package it depends on
    ({!Odoc_unit.pkg.deps}) is ready, and packages that do not depend on one
    another are built at the same time. Within a package, a library is compiled
    after the libraries of the package among its transitive dependencies
    ({!Odoc_unit.lib.lib_includes}), each once.

    A package with doc deps ({!Odoc_unit.pkg.doc_deps}) is split, as ocaml.org
    splits it: it is compiled, then its doc deps are compiled, and only then is
    it linked. A split package is ready once it is compiled, and any other once
    it is linked and rendered.

    The packages with no name, such as the top-level index, are built after the
    others. If the packages wait for one another in a cycle, it fails, naming
    them, before it builds anything. *)
