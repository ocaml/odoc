type library = {
  name : string;
  archive_name : string option;
  dir : string option;
  deps : string list;
}

val process_meta_file : Fpath.t -> library list
(** From a path to a [Meta] file, returns the list of libraries defined in this
    file. *)
