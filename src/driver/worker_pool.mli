val submit : string -> Bos.Cmd.t -> Fpath.t option -> (Run.t, exn) result
(** Submit a command to be executed by a worker.

    [submit desc cmd output_file] returns the list of output lines. [desc] is a
    description of the command. *)

val start_workers : Eio_unix.Stdenv.base -> Eio.Switch.t -> int -> unit
(** Start the given number of new workers. *)
