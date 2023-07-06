open Types

val anchor_of_id : string -> Odoc_model.Names.DefName.t
(** Returns the anchor that will be used to link to the [id]. *)

val unpack_uid : Uid.t -> (string * string option) option
(** [unpack_uid uid] unpacks a [uid] into [Some (comp_unit, id)] *)
