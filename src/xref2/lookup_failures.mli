(** Report non-fatal errors.

    This is internally using {!Odoc_model.Error}. The main difference is that no
    precise location is attached to each failures, instead a filename is given
    to {!catch_failures}.

    Each failure has a [kind] which specify whether it's a lookup failure
    ([`Root] or [`Internal]) or a warning. [`Root] failures are never turned
    into fatal warnings. *)

open Odoc_model

type kind = [ `Root | `Internal | `Warning ]
(** [`Root] failures won't be turned into fatal warnings. [`Internal] is for
    lookup failures other than root modules and [`Warning] for messages to the
    users. They may be turned into fatal warnings depending on [~warn_error]. *)

val catch_failures : filename:string -> (unit -> 'a) -> 'a Error.with_warnings
(** Catch failures thrown by [report]. [filename] is the initial location of
    generated errors, more precise locations can be specified with
    [with_location]. *)

val report : ?kind:kind -> ('fmt, Format.formatter, unit, unit) format4 -> 'fmt
(** Report a lookup failure to the enclosing [catch_failures] call. *)

val with_location : Location_.span -> (unit -> 'a) -> 'a
(** Failures reported indirectly by this function will have a location attached. *)
