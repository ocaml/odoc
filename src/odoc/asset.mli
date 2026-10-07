open Odoc_utils

val compile :
  parent_id:string ->
  name:string ->
  output_dir:string option ->
  output_file:string option ->
  (unit, [> msg ]) result
(** [output_file] overrides the path computed from [parent_id] and [output_dir],
    as [-o] does for {!Compile.compile}. *)
