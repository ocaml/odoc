open Odoc_model.Lang.Source_code.Info

type local_jmp_infos = jmp_to_def with_pos list

val of_source : local_jmp:local_jmp_infos -> string -> info with_pos list
