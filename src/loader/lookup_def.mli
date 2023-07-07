open Odoc_model
open Paths
type t

val lookup_def :
  (string -> (Lang.Compilation_unit.t * t) option) ->
  Identifier.NonSrc.t ->
  Identifier.SourceLocation.t option
(** Returns the root module containing the definition of the given identifier
    and the corresponding anchor. *)

val of_cmt : Cmt_format.cmt_infos -> Paths.Identifier.SourcePage.t option -> t option
(** Returns [None] if the cmt doesn't have a shape (eg. if it is not an
    implementation). Returns [Some _] even if shapes are not implemented.

    In case of [Some _], returns both the shape and the relevant infos taken
    from the [cmt]. *)

#if OCAML_VERSION >= (4, 14, 0)

val id_of_uid : t -> Shape.Uid.t -> Paths.Identifier.SourceLocation.t option

#endif