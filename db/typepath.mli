(** This module contains the transformation that make types searchable.

A type can viewed as a tree. [a -> b -> c * d] is the following tree :
{[ ->
    |- a
    |- ->
        |- b
        |- *
           |- c
           |- d
]}
To make types searchable, we consider the list of paths from root to leaf in the
tree of the type.

Here the paths would be : [ [[-> a]; [-> -> b]; [-> -> * c ]; [-> -> * d]] ]

There are two submodules, that each encode slightly different information from
the above. *)

val regroup : string list list -> (string list * int) list

module Sign : sig
  type t =
    | Pos
    | Neg
    | Unknown

  val to_string : t -> string
  val not : t -> t
end

module For_suffix_tree : sig
  type t = string list list
  (** [For_suffix_tree.t] is a type paths that can used to be register in a
      suffix tree. This means that each path represent a text based searchable
      version of the type. The chosen representation here is polarity : we do
      not represent the [->] or the [*] constructors, but they affect the
      "polarity" of their children.

      The polarity of a of component of a type indicate if it is produced or
      consumed by the type. In the type [int -> string], [int] has negative
      polarity because it is being consumed, and [string] has positive polarity
      because it is being produced.

      When you have [t -> u], the polarity of [t] is inversed, and the
      polarity of [u] stays the same. So when you have

    If you consider [a -> b -> c * d] with the following tree :
    {[ ->
        |- a
        |- ->
            |- b
            |- *
               |- c
               |- d
    ]}

    The [For_distance.t] associated is : [ [[- a]; [-; b]; [+; c ]; [+; d]] ]

    [babar : Lib.M.t -> Lib.R.t]

    [[-; Lib.M.t]; [+;Lib.R.t]; [-;M.t]; [-;t]; [+;R.t]; [+;t]]
 *)

  val of_typ : ignore_any:bool -> all_names:bool -> Typexpr.t -> t
end

(* module For_distance : sig
  type t = string list list
  (** [For_distance.t] is a type paths that can used to compute the distance
      between two types. It is much more precise than {!For_suffix.t}, we do not
      lose any information about the type. Because of this, we represent [->]
      and [*] add annotations to indicate the child's position relative to its
      parent (first child or second child ?)

    If you consider [a -> b -> c * d] with the following tree :
    {[ ->
        |- a
        |- ->
            |- b
            |- *
               |- c
               |- d
    ]}

    The [For_distance.t] associated is : [ [[-> 1 a]; [-> 2 -> 1 b]; [-> 2 -> 2
    * 1 c ]; [-> 2 -> 2 * 2 d]] ]
 *)

  val of_typ : ignore_any:bool -> Typexpr.t -> t
end *)
