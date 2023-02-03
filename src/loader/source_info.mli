open Odoc_model.Lang.Source_info

type local_jmp_infos = jmp_to_def with_pos list

val of_local_jmp : local_jmp_infos -> infos
(** Source infos loaded from the cmt file. *)

val of_source : string -> infos
(** Source infos parsed from the source code. *)
