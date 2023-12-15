(** This module provide a way to transform a type into strings, in such a way
  that the string can be used for type search.

The chosen representation is polarity : we do not represent the [->] or the [*]
constructors, but they affect the "polarity" of their children.

The polarity of a component of a type indicate if it is produced or consumed by
the type. In the type [int -> string], [int] has negative polarity because it is
being consumed, and [string] has positive polarity because it is being produced.

When you have [t -> u], the polarity of [t] is inversed, and the polarity of [u]
stays the same. So in the type [(int -> string) -> float], the polarities are
[+int], [-string] and [+float]. This is easier to see if you write the type with
an infix notation for the arrow : [-> (-> int string) float].

Once you have computed the polarities of the type of an entry [e], you can
register each polarity as corresponding to [e] in the search database. Then,
when the user queries for a type, we compute the polarities of the query type, a
search in a database for every polarity.

We then return the result corresponding to intersection of each polarity : if
the user queries for [int -> string], we want to have every entry which consumes
an [int] and produces a [string], that is the intersection of the entries
associated to [-int] with the entries associated to [+string].

There is however a complication. If the user queries for [int -> int -> string],
then the polarities will be [-int], [-int] and [+string]. An entry of type [int
-> string] would be included in the intersection of these polarities. But the
user explicitely asked for two integer to be consumed. To fix this issue, we
track the number of occurences of each polarity.

The polarities for [int -> int -> string], become [("-int", 2)] and [("+string",
1)], and allows us to filter entries according to this information. The exact
mechanism for this is explained in {!Occ}.
*)

module Sign : sig
  type t =
    | Pos
    | Neg

  val to_string : t -> string
  val not : t -> t
end

type t = string * int

val of_typ : ignore_any:bool -> all_names:bool -> Typexpr.t -> t list
(** [of_typ ~ignore_any ~all_names typ] is the list of polarised types
    corresponding to [typ].

  - If [ignore_any] is true, the type [_] will be ignored, if it is true, it
    will be treated like a type variable ['a].

  - If [all_names] is true, extra polarities are added for every "possible name"
    of each type. For instance the possible names of [Stdlib.Int64.t] are ["t"],
    ["Int64.t"] and ["Stdlib.Int64.t"]. This allows for the user to use any of
    the possible name. It is important to set this when registering entries in
    the database, but you not need it when computing the polarities of a query.
    *)
