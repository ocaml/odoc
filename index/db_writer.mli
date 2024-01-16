type t
(** The type that builds a database. You can use it to add things to it, but
    you cannot make queries on it. *)

val export : summarize:bool -> t -> Db.t

val make : unit -> t
(** [make ()] returns an empty search database. *)

val load : t -> int
val type_of_odoc : db:t -> Odoc_model.Lang.TypeExpr.t -> Db.Typexpr.t
val store_type_polarities : t -> Db.Entry.t -> Db.Type_polarity.t Seq.t -> unit
val store_word : t -> string -> Db.Entry.t -> unit
