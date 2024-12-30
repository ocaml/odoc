(** Locations in files. *)

(** This module concerns locations in source files, both points indicating a specific
    character and spans between two points. *)

(** {2 Basic types} *)

type point = { line : int; column : int }
(** A specific character *)

val dummy_pos : point

type span = { file : string; start : point; end_ : point }
(** A range of characters between [start] and [end_] in a particular file *)

val span : span list -> span
(** [span spans] takes a list of spans and returns a single {!type-span} starting
    at the start of the first span and ending at the end of the final span *)

val nudge_start : int -> span -> span
(** This adjusts only the column number, implicitly assuming that the offset does
   not move the location across a newline character. *)

val nudge_end : int -> span -> span
(** This adjusts only the column number, implicitly assuming that the offset does
   not move the location across a newline character. *)

(** {2 Located values} *)

type +'a with_location = { location : span; value : 'a }
(** Describes values located at a particular span *)

val fmt : span -> string
(** Format a `span` and return the resulting string *)

val delimited : 'a with_location -> 'b with_location -> span
(** Returns the span which contains both arguments *)

val nudge_map_start : int -> 'a with_location -> 'a with_location
val nudge_map_end : int -> 'a with_location -> 'a with_location

val at : span -> 'a -> 'a with_location
(** Constructor for {!with_location} *)

val of_position : ?filename:string -> Lexing.position * Lexing.position -> span
(** Convert Menhir's `$loc` or `$sloc` into a span *)

val extract :
  input:string -> start_pos:Lexing.position -> end_pos:Lexing.position -> string
(** Given a starting positon and an ending position extract the portion of the
    input text between those positions *)

val location : 'a with_location -> span
(** Returns the location of a located value *)

val value : 'a with_location -> 'a
(** Returns the value of a located value *)

val map : ('a -> 'b) -> 'a with_location -> 'b with_location
(** Map over a located value without changing its location *)

val is : ('a -> bool) -> 'a with_location -> bool
(** Test a located value with a predicate *)

val same : _ with_location -> 'b -> 'b with_location
(** [same x y] retuns the value y wrapped in a {!with_location} whose
    location is that of [x] *)

val spans_multiple_lines : _ with_location -> bool
(** [spans_multiple_lines x] checks to see whether [x] is located
    on a single line or whether it covers more than one. *)
