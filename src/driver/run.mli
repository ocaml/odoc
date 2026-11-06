(** Running external commands.

    Every command the driver runs goes through {!run}, which records what was
    run, how long it took and what it printed. The records feed the statistics
    ({!Stats}) and the messages the driver prints at the end. *)

type t = {
  cmd : string list;  (** The command and its arguments. *)
  time : float;  (** How long the command ran, in seconds. *)
  output_file : Fpath.t option;
      (** The file the command was expected to write. *)
  output : string;  (** What the command wrote to standard output. *)
  errors : string;  (** What the command wrote to standard error. *)
  status : [ `Exited of int | `Signaled of int ];
}

val run : Eio_unix.Stdenv.base -> Bos.Cmd.t -> Fpath.t option -> t
(** [run env cmd output_file] runs [cmd] to completion and records the result. A
    command that does not exit with code 0 is reported as an error. *)

val filter_commands : string -> t list
(** [filter_commands sub] returns the recorded runs whose first argument was
    [sub], for example ["compile"] or ["link"] for the odoc subcommands. *)
