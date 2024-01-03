(** This module provide a way to transform a type into strings, in such a way
  that the strings can be used for type search.

The chosen representation is polarity : we do not represent the [->] or the [*]
constructors, but instead compute the "polarity" of every type name/constructor
like [int] or ['a] that is part of the whole type expression.

The polarity of a component of a type indicates if it is produced or consumed by
the type. In the type [int -> string], [int] has negative polarity because it is
being consumed, and [string] has positive polarity because it is being produced.
We say that the polarities of [int -> string] are [-int] and [+string].

Once you have computed the polarities of the type of an entry [e], you can
register each polarity as corresponding to [e] in the search database.

Then, when the user queries for a type, we compute the polarities of the query
type, and search for the entries. The

We then return the result corresponding to intersection of each polarity: if the
user queries for [int -> string], we want to have every entry which consumes an
[int] and produces a [string], that is the intersection of the entries
associated to [-int] with the entries associated to [+string].

How is polarity computed exactly ? When you have [t -> u], the polarity of [t]
is inversed, and the polarity of [u] stays the same. A good example of this is
the type of {!Stdlib.Out_channel.with_open_gen} :

{| val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a |}

Here the polarities are [-open_flag list], [-int], [-string], [+Out_channel.t],
[-'a] and [+'a]. The fact that we have [+Out_channel.t] might be puzzling at
first, because an [Out_channel.t] is not returned by the function, but
{!Stdlib.Out_channel.with_open_gen} is indeed one of the possible ways to create
an [Out_channel.t].

There is however a complication. If the user queries for [int -> int -> string],
then the polarities will be [-int], [-int] and [+string]. An entry of type [int
-> string] would be included in the intersection of these polarities. But the
user explicitely asked for two integers to be consumed. To fix this issue, we
track the number of occurences of each polarity.

The polarities for [int -> int -> string], become [(-int, 2)] and [(+string,
1)], and allows us to filter entries according to this information. The exact
mechanism for this is explained in {!Occ}.

There is a mechanism for types with parameters like ['a list]. I might explain
it in the future.
*)

module Sign : sig
  type t =
    | Pos
    | Neg

  val to_string : t -> string
  val not : t -> t
end

type t = string * int * Sign.t
(** The search database is a suffix tree structure, implemented in
    {!Suffix_tree}. It is a solely text-based datastructure. Therefore, we need
    a text represention for the polarities.

    The polarity [+t] is represented by ["+t"], and the polarity [-t] is
    represented by ["-t"].

    The fact that the sign is in the front is important : ["+flo"] is a prefix of
    ["+float"], but ["flo+"] is not a prefix nor a suffix of ["float+"]. This
    allows to answer incomplete queries.

    The integer represents the occurences of the polarity, as explained in the
    toplevel documentation of the module. *)

val of_typ : any_is_poly:bool -> all_names:bool -> Typexpr.t -> t Seq.t
(** [of_typ ~ignore_any ~all_names typ] is the list of polarised types
    corresponding to [typ].

    - If [any_is_poly] is true, the type [_] will be treated like a type variable
      ['a], other it will be represented solely by its sign ("+" or "-").

    - If [all_names] is true, extra polarities are added for every "possible name"
      of each type constructor. For instance the possible names of
      [Stdlib.Int64.t] are ["t"], ["Int64.t"] and ["Stdlib.Int64.t"]. This allows
      for the user to use any of the possible name. It is important to set this
      when registering entries in the database, but you not need it when computing
      the polarities of a query. *)
