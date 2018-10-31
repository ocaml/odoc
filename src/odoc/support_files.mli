(** Copies odoc's support files (default theme and JS files) to a specified
    location. *)

val write : ?without_theme: bool -> Fs.Directory.t -> unit
(** [write ?without_theme output_dir] copies the support files to the
    [output_dir]. If [without_theme] is [true] the theme will {e not} be
    copied, the default value is [false]. *)

val filenames : string list
(** The list of the names of the files that calling [Support_files.write] would
    output. *)
