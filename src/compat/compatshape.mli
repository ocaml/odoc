open Odoc_model.Paths

type impl_shape

val lookup_def : impl_shape -> Identifier.t -> Location.t option

val of_cmt : Cmt_format.cmt_infos -> impl_shape option
(** Returns [None] if the cmt doesn't have a shape (eg. if it is not an
    implementation). Returns [Some _] even if shapes are not implemented. *)
