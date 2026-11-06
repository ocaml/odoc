(** The [status.json] file ocaml.org reads for each package.

    It lists the package's HTML files and any redirections from old page
    locations to new ones. *)

val file :
  html_dir:Fpath.t ->
  pkg:Packages.t ->
  ?redirections:(Fpath.t, Fpath.t) Hashtbl.t ->
  unit ->
  unit
(** [file ~html_dir ~pkg ()] writes [status.json] in the package's directory
    below [html_dir]. [redirections] maps old paths to new ones, both relative
    to the package's directory. *)
