(** Report non-fatal errors *)

type 'a with_failures
(** A value that may be partially unresolved due to failures. *)

val catch_failures : (unit -> 'a) -> 'a with_failures

val report : ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Report a lookup failure to the enclosing [catch_failures] call. *)

val report_important :
  exn -> ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Like [report] above but may raise the exception [exn] if strict mode is
    enabled *)

val to_warning :
  filename:string -> 'a with_failures -> 'a Odoc_model.Error.with_warnings
(** Convert the failures to a warning. *)
