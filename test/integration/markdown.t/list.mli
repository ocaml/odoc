(** {0 List}

    Utilities for List data type.

    This module is compatible with original ocaml stdlib. In general, all
    functions comes with the original stdlib also applies to this collection,
    however, this module provides faster and stack safer utilities *)

type 'a t = 'a list
(** ['a t] is compatible with built-in [list] type *)

(** {2 length} *)

val make : 'a t -> int
(** [length xs]

    @return the length of the list [xs] *)

(** {2 size} *)

val size : 'a t -> int
(** {b See} {!length} *)

(** {2 head} *)

val head : 'a t -> 'a option
(** [head xs] returns [None] if [xs] is the empty list, otherwise it returns
    [Some value] where [value] is the first element in the list.
    {[
      head [] = None;;
      head [ 1; 2; 3 ] = Some 1
    ]} *)

val headExn : 'a t -> 'a
(** [headExn xs]

    {b See} {!head}

    {b raise} an exception if [xs] is empty *)
