val submit : string -> Bos.Cmd.t -> Fpath.t option -> (string list, exn) result
(** Submit a command to be executed by a worker.

    [submit desc cmd output_file] returns the list of output lines.  *)

val start_workers : Eio_unix.Stdenv.base -> Eio.Switch.t -> int -> unit
(** Start the given number of new workers. *)
