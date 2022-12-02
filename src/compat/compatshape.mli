open Odoc_model.Paths

type impl_shape

val lookup_def : impl_shape -> Identifier.t -> Location.t option

val of_cmt :
  Cmt_format.cmt_infos -> (impl_shape * Source_info.Types.infos) option
(** Returns [None] if the cmt doesn't have a shape (eg. if it is not an
    implementation). Returns [Some _] even if shapes are not implemented.

    In case of [Some _], returns both the shape and the relevant infos taken
    from the [cmt]. *)
