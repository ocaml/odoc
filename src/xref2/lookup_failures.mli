(** Report non-fatal errors.

    The main difference with {!Odoc_model.Error} is that no precise location is
    attached to each failures, instead a filename is given to {!catch_failures}. *)

open Odoc_model

val catch_failures : filename:string -> (unit -> 'a) -> 'a Error.with_warnings
(** Catch failures that are reported by [f]. [filename] is the initial location
    of generated errors, more precise locations can be specified with
    [with_location]. *)

val report_internal : ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Internal errors happens during compiling and linking. *)

val report_root : name:string -> unit
(** Root errors happens when a dependency couldn't be loaded. These errors won't
    be made fatal in "warn error" mode. *)

val report_warning : ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Warnings are user errors. *)

val with_location : Location_.span -> (unit -> 'a) -> 'a
(** Failures reported indirectly by this function will have a location attached. *)

val with_context :
  ('fmt, Format.formatter, unit, (unit -> 'a) -> 'a) format4 -> 'fmt
(** [with_context "format string" format_arguments f] adds context to failures
    reported by [f ()]. *)
