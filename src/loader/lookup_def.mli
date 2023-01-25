open Odoc_model
open Paths
type t

val lookup_def :
  (string -> (Lang.Compilation_unit.t * t) option) ->
  Identifier.t ->
  Lang.Locations.t option
(** Returns the root module containing the definition of the given identifier
    and the corresponding anchor. *)

val of_cmt : Cmt_format.cmt_infos -> t option
(** Returns [None] if the cmt doesn't have a shape (eg. if it is not an
    implementation). Returns [Some _] even if shapes are not implemented.

    In case of [Some _], returns both the shape and the relevant infos taken
    from the [cmt]. *)
