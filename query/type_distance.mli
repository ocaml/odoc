
(** [Type_distance.v ~query ~entry] is an integer representing a notion of
    distance between two types. [query] is a type from a query, and [entry] is
    the type of a possible response to this query. *)
val v : query:Db.Typexpr.t -> entry:Db.Typexpr.t -> int
