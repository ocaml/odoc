(** Progress display and statistics.

    The build steps ({!module-Compile}) and the {!Worker_pool} report their
    progress here, and the progress display reads it. *)

(** {1 Progress} *)

type phase =
  | Compile  (** [odoc compile], of any kind of unit. *)
  | Link
  | Index  (** A package's index, sidebar and search database. *)
  | Generate  (** The output of one unit, HTML and JSON alike. *)

val expect : phase -> int -> unit
(** [expect phase n]: [n] more units will pass through [phase]. *)

val did : phase -> unit
(** One unit has passed through the phase. *)

(** {1 Workers} *)

val init_nprocs : int -> unit
(** Size the per-worker activity table. Call before starting the workers. *)

val worker_busy : int -> string -> unit
(** [worker_busy id description]: worker [id] has started a command. *)

val worker_idle : int -> unit
(** Worker [id] has finished its command, however it ended. *)

(** {1 Display} *)

val finish : unit -> unit
(** The build is complete. The display stops after its next update. *)

val render_stats : Eio_unix.Stdenv.base -> int -> unit
(** Show a progress bar for each phase and for the running commands, until
    {!finish} is called. Meant to run in a fiber alongside the build. *)

val bench_results : Fpath.t -> unit
(** Write [driver-benchmarks.json] in the current directory. It holds the
    timings of this run's odoc commands, and the size of the output below the
    given HTML directory. *)
