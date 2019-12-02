(** Re-export for compatibility with 4.02 *)
type ('a, 'e) result = ('a, 'e) Result.result =
  | Ok of 'a
  | Error of 'e

type 'a or_error = ('a, [ `Msg of string ]) Result.result

val (>>=) : 'a or_error -> ('a -> 'b or_error) -> 'b or_error
