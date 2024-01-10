module Type_path : sig
  type t

  val of_typ : ignore_any:bool -> Db.Typexpr.t -> t
end

val v : query_paths:Type_path.t -> entry:Db.Typexpr.t -> int
(** [Type_distance.v ~query_paths ~entry] is an integer representing a notion of
    distance between two types. [query_paths] is a type from a query, and [entry] is
    the type of a possible candidate for this query. *)
