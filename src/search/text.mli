(** This module is used to turn values from odoc model into a more easily
    searchable format: text! *)

val of_type : Odoc_model.Lang.TypeExpr.t -> string

val of_doc : Odoc_model.Comment.docs -> string

val of_record : Odoc_model.Lang.TypeDecl.Field.t list -> string
