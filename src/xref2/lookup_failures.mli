(** Report non-fatal errors *)

type kind = [ `Root | `Internal ]

type 'a with_failures
(** A value that may be partially unresolved due to failures. *)

val catch_failures : (unit -> 'a) -> 'a with_failures

val report : ?kind:kind -> ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Report a lookup failure to the enclosing [catch_failures] call. *)

val report_important :
  ?kind:kind -> exn -> ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Like [report] above but may raise the exception [exn] if strict mode is
    enabled *)

val handle_failures :
  warn_error:bool ->
  filename:string ->
  'a with_failures ->
  ('a, [> `Msg of string ]) Result.result
(** Print failures to stderr. Some failures may be turned into warnings,
    [Error.handle_warnings] will be called to print them. *)
