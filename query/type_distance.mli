type t

val paths_of_type : Db.Typexpr.t -> t

val v : query_paths:t -> entry:Db.Typexpr.t -> int
(** [Type_distance.v ~query_paths ~entry] is an integer representing a notion of
    distance between two types. [query_paths] is a type from a query, and [entry] is
    the type of a possible candidate for this query. *)
