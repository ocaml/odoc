(** A fixed pool of workers that run external commands.

    The driver runs many odoc commands concurrently. It submits them to the
    pool, which runs at most one command per worker at a time. *)

exception Worker_failure of Run.t
(** Raised through {!submit} when a command does not exit with code 0. *)

val start_workers : Eio_unix.Stdenv.base -> Eio.Switch.t -> int -> unit
(** Start the given number of workers. Call this once, before any command is
    submitted. *)

val submit : string -> Bos.Cmd.t -> Fpath.t option -> (Run.t, exn) result
(** [submit description cmd output_file] runs [cmd] on a worker and waits for
    it. [description] is shown in the progress display. [output_file] is the
    file the command is expected to write, for the statistics. *)
