open Odoc_model
open Odoc_model.Comment

val inline_element : inline_element Location_.with_location list Type_desc.t

val elements : elements Type_desc.t

val docs : docs Type_desc.t

val docs_or_stop : docs_or_stop Type_desc.t
