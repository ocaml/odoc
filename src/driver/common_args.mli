(** Command-line arguments shared by the drivers. *)

val fpath_arg : Fpath.t Cmdliner.Arg.conv

type t = {
  verbose : bool;
  html_dir : Fpath.t;  (** Where the HTML goes. *)
  stats : bool;  (** Write [driver-benchmarks.json] at the end. *)
  nb_workers : int;  (** How many commands run at once. *)
  odoc_bin : string option;
      (** The [odoc] binary to run, if not the one in PATH. *)
  odoc_md_bin : string option;
      (** The [odoc-md] binary to run, if not the one in PATH. *)
  generate_json : bool;  (** Also write the pages as JSON. *)
}
(** Options common to all drivers. *)

val term : t Cmdliner.Term.t

val setup : t -> unit
(** Act on the options: which odoc to run, how much to log, and how many workers
    to show. *)

type dirs = {
  odoc_dir : Fpath.t option;  (** [.odoc] files. *)
  odocl_dir : Fpath.t option;  (** [.odocl] files. *)
  mld_dir : Fpath.t option;  (** The [.mld] files the driver writes itself. *)
  index_dir : Fpath.t option;  (** Index and sidebar files. *)
}
(** The directories for intermediate files. Each is optional; a missing one is
    replaced by a temporary directory for the run (see {!with_dirs}). *)

val dirs_term : dirs Cmdliner.Term.t

val with_dirs :
  dirs ->
  (odoc_dir:Fpath.t ->
  odocl_dir:Fpath.t ->
  index_dir:Fpath.t ->
  mld_dir:Fpath.t ->
  unit ->
  unit) ->
  unit
(** [with_dirs dirs f] calls [f] with a directory for each kind of intermediate
    file, creating temporary directories for those not given. *)
