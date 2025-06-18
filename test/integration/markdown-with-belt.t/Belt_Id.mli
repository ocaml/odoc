(** {!Belt.Id}

    Provide utilities to create identified comparators or hashes for data
    structures used below.

    It create a unique identifier per module of functions so that different data
    structures with slightly different comparison functions won't mix *)

type t
(** [t] is the type of the identifier *)
