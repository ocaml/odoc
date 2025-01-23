type t

val make : unit -> t
val of_odoc : cache:t -> Odoc_model.Lang.TypeExpr.t -> Db.Typexpr.t
