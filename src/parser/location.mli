type point = { line : int; column : int }

type span = { file : string; start : point; end_ : point }

type +'a with_location = { location : span; value : 'a }

val at : span -> 'a -> 'a with_location

val location : 'a with_location -> span

val value : 'a with_location -> 'a

val map : ('a -> 'b) -> 'a with_location -> 'b with_location

val same : _ with_location -> 'b -> 'b with_location

val span : span list -> span

(* This adjusts only the column number, implicitly assuming that the offset does
   not move the location across a newline character. *)
val nudge_start : int -> span -> span

