(** Running commands and keeping their output for the final report.

    The driver prints the warnings that odoc emitted only at the end of a run,
    grouped by command. This module submits commands to the {!Worker_pool} and
    keeps the output of those the caller asks to log. *)

type log_dest =
  [ `Compile
  | `Link
  | `Count_occurrences
  | `Generate
  | `Index
  | `Sherlodoc
  | `Classify ]
(** The step a logged command belongs to. *)

type log_line = {
  log_dest : log_dest;
  prefix : string;
      (** Identifies the command in the report, usually its input file. *)
  run : Run.t;
}

val outputs : log_line list ref
(** The logged commands, in the order they finished. *)

val submit :
  (log_dest * string) option ->
  string ->
  Bos.Cmd.t ->
  Fpath.t option ->
  string list
(** [submit log description cmd output_file] runs [cmd] on a worker and returns
    the lines it wrote to standard output. When [log] is [Some (dest, prefix)],
    the run is added to {!outputs}. Raises {!Worker_pool.Worker_failure} if the
    command fails. *)

type action = {
  log : (log_dest * string) option;
  desc : string;  (** What the command does, for the progress display. *)
  cmd : Bos.Cmd.t;
  output : Fpath.t option;
      (** The file the command writes, if it writes one. *)
  ignore_failures : bool;
}
(** One command the driver has decided to run. Building the command and running
    it are separate steps. *)

val v :
  ?log:log_dest * string ->
  ?output:Fpath.t ->
  ?ignore_failures:bool ->
  desc:string ->
  Bos.Cmd.t ->
  action
(** An action. By default it is not logged, writes no file, and a failure is
    fatal. *)

val run : action -> unit
(** Run the command on a worker, and keep its output if it is logged. *)
