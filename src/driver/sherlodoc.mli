(** Running [sherlodoc], which provides the search boxes of the generated pages.
    Paths are relative to the HTML directory unless stated otherwise. *)

val js_file : Fpath.t
(** The search engine's JavaScript, shared by all packages. *)

val db_js_file : Fpath.t -> Fpath.t
(** [db_js_file dir] is the search database of a package, in the package's HTML
    directory [dir]. *)

val js : Fpath.t -> Cmd_outputs.action
(** [js dst] writes the search engine's JavaScript to [dst], an absolute path.
*)

val index : inputs:Fpath.t list -> dst:Fpath.t -> Cmd_outputs.action
(** [index ~inputs ~dst] builds the JavaScript search database [dst] from the
    given [.odoc-index] files. *)
